use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process::ExitCode;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Severity {
    Error,
    Warning,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Diagnostic {
    line: usize,
    severity: Severity,
    code: &'static str,
    message: String,
}

impl Diagnostic {
    fn error(line: usize, code: &'static str, message: impl Into<String>) -> Self {
        Self {
            line,
            severity: Severity::Error,
            code,
            message: message.into(),
        }
    }

    fn warning(line: usize, code: &'static str, message: impl Into<String>) -> Self {
        Self {
            line,
            severity: Severity::Warning,
            code,
            message: message.into(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Part {
    Decimal(u32),
    Alpha(char),
    Roman(u32),
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Clause(Vec<Part>);

impl Clause {
    fn render(&self) -> String {
        let mut result = String::new();
        for part in &self.0 {
            match part {
                Part::Decimal(n) if result.is_empty() => result.push_str(&n.to_string()),
                Part::Decimal(n) => result.push_str(&format!(".{n}")),
                Part::Alpha(c) if c.is_ascii_uppercase() => result.push(*c),
                Part::Alpha(c) => result.push_str(&format!("({c})")),
                Part::Roman(n) => result.push_str(&format!("({})", roman(*n))),
            }
        }
        result
    }

    fn parent(&self) -> Option<Self> {
        if matches!(self.0.as_slice(), [Part::Decimal(_), Part::Alpha(c)] if c.is_ascii_uppercase())
        {
            None
        } else if self.0.len() > 1 {
            Some(Self(self.0[..self.0.len() - 1].to_vec()))
        } else {
            None
        }
    }

    fn depth(&self) -> usize {
        if matches!(self.0.get(1), Some(Part::Alpha(c)) if c.is_ascii_uppercase()) {
            self.0.len() - 1
        } else {
            self.0.len()
        }
    }
}

fn roman(mut n: u32) -> String {
    let mut result = String::new();
    for (value, text) in [
        (1000, "m"),
        (900, "cm"),
        (500, "d"),
        (400, "cd"),
        (100, "c"),
        (90, "xc"),
        (50, "l"),
        (40, "xl"),
        (10, "x"),
        (9, "ix"),
        (5, "v"),
        (4, "iv"),
        (1, "i"),
    ] {
        while n >= value {
            result.push_str(text);
            n -= value;
        }
    }
    result
}

fn parse_roman(text: &str) -> Result<u32, &'static str> {
    let values: Vec<u32> = text
        .chars()
        .map(|c| match c {
            'i' => 1,
            'v' => 5,
            'x' => 10,
            'l' => 50,
            'c' => 100,
            'd' => 500,
            'm' => 1000,
            _ => 0,
        })
        .collect();
    if values.is_empty() || values.contains(&0) {
        return Err("invalid lower-case Roman numeral");
    }
    let n = values
        .iter()
        .enumerate()
        .map(|(i, v)| {
            if values.get(i + 1).is_some_and(|next| v < next) {
                -(*v as i64)
            } else {
                *v as i64
            }
        })
        .sum::<i64>();
    if n <= 0 || roman(n as u32) != text {
        return Err("invalid lower-case Roman numeral");
    }
    Ok(n as u32)
}

fn parse_decimal(text: &str) -> Result<u32, &'static str> {
    if text.is_empty() || text.starts_with('0') {
        return Err("decimal clause parts must be positive integers without leading zeroes");
    }
    text.parse::<u32>()
        .ok()
        .filter(|n| *n > 0)
        .ok_or("decimal clause parts must be positive integers without leading zeroes")
}

fn take_digits(text: &str) -> (&str, &str) {
    let end = text.bytes().take_while(u8::is_ascii_digit).count();
    text.split_at(end)
}

fn parenthesised(text: &str) -> Result<(&str, &str), &'static str> {
    let rest = text
        .strip_prefix('(')
        .ok_or("unexpected characters in clause identifier")?;
    let (inside, after) = rest
        .split_once(')')
        .ok_or("unclosed or empty parenthesised clause part")?;
    if inside.is_empty() {
        Err("unclosed or empty parenthesised clause part")
    } else {
        Ok((inside, after))
    }
}

fn parse_clause(token: &str) -> Result<Clause, &'static str> {
    let (number, mut rest) = take_digits(token);
    let mut parts = vec![Part::Decimal(parse_decimal(number)?)];
    if rest.starts_with(|c: char| c.is_ascii_uppercase()) {
        let (letter, after) = rest.split_at(1);
        parts.push(Part::Alpha(letter.chars().next().unwrap()));
        rest = after;
    }
    while let Some(after_dot) = rest.strip_prefix('.') {
        let (number, after) = take_digits(after_dot);
        parts.push(Part::Decimal(parse_decimal(number)?));
        rest = after;
    }
    if !rest.is_empty() {
        let (letter, after) = parenthesised(rest)?;
        if letter.len() != 1 || !letter.starts_with(|c: char| c.is_ascii_lowercase()) {
            return Err("the first parenthesised clause part must be one lower-case letter");
        }
        parts.push(Part::Alpha(letter.chars().next().unwrap()));
        if !after.is_empty() {
            let (numeral, trailing) = parenthesised(after)?;
            parts.push(Part::Roman(parse_roman(numeral)?));
            if !trailing.is_empty() {
                return Err("too many parenthesised clause parts");
            }
        }
    }
    Ok(Clause(parts))
}

#[derive(Clone)]
struct Heading {
    line: usize,
    level: usize,
    text: String,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum ListKind {
    Ordered,
    Unordered,
}

struct ListFrame {
    indent: usize,
    kind: ListKind,
    next: u32,
}

#[derive(Default)]
struct Scan {
    fence: Option<(char, usize)>,
    comment: bool,
    headings: Vec<Heading>,
    clauses: Vec<(usize, Clause)>,
    visible: Vec<(usize, String)>,
    lists: Vec<ListFrame>,
    ordered_items: usize,
    first_top_ordered: Option<usize>,
    diagnostics: Vec<Diagnostic>,
    last_line: usize,
}

fn strip_comments(mut inside: bool, mut text: &str) -> (String, bool) {
    let mut output = String::new();
    loop {
        if inside {
            if let Some((_, rest)) = text.split_once("-->") {
                text = rest;
                inside = false;
            } else {
                return (output, true);
            }
        } else if let Some((before, rest)) = text.split_once("<!--") {
            output.push_str(before);
            text = rest;
            inside = true;
        } else {
            output.push_str(text);
            return (output, false);
        }
    }
}

fn fence_marker(text: &str) -> Option<(char, usize)> {
    let text = text.trim_start();
    let marker = text.chars().next()?;
    if marker != '`' && marker != '~' {
        return None;
    }
    let width = text.chars().take_while(|c| *c == marker).count();
    (width >= 3).then_some((marker, width))
}

fn parse_heading(text: &str) -> Option<Result<(usize, String), &'static str>> {
    let indent = text.bytes().take_while(|b| *b == b' ').count();
    if indent > 3 {
        return None;
    }
    let text = &text[indent..];
    let level = text.bytes().take_while(|b| *b == b'#').count();
    if level == 0 {
        return None;
    }
    let rest = &text[level..];
    if level > 6 || (!rest.is_empty() && !rest.starts_with(' ')) {
        return Some(Err("malformed ATX heading"));
    }
    let title = rest.trim().trim_end_matches('#').trim();
    Some(if title.is_empty() {
        Err("heading is empty")
    } else {
        Ok((level, title.to_owned()))
    })
}

fn parse_list(text: &str) -> Option<(usize, ListKind, u32)> {
    let indent = text.bytes().take_while(|b| *b == b' ').count();
    let text = &text[indent..];
    if matches!(text.as_bytes(), [b'-' | b'*' | b'+', b' ', ..]) {
        return Some((indent, ListKind::Unordered, 0));
    }
    let (digits, after) = take_digits(text);
    if after.starts_with(". ") {
        digits.parse().ok().map(|n| (indent, ListKind::Ordered, n))
    } else {
        None
    }
}

fn bold_clause_like(text: &str) -> bool {
    let mut text = text.trim_start();
    loop {
        if text.starts_with("**") {
            return text[2..].starts_with(|c: char| c.is_ascii_digit());
        }
        if matches!(text.as_bytes(), [b'-' | b'*' | b'+', b' ', ..]) {
            text = &text[2..];
            continue;
        }
        let (digits, after) = take_digits(text);
        if !digits.is_empty() && after.starts_with(". ") {
            text = &after[2..];
            continue;
        }
        return false;
    }
}

fn update_list(scan: &mut Scan, line: usize, indent: usize, kind: ListKind, number: u32) {
    if let Some(index) = scan.lists.iter().position(|frame| frame.indent == indent) {
        scan.lists.truncate(index + 1);
        let frame = &mut scan.lists[index];
        if frame.kind == kind {
            if kind == ListKind::Ordered && number != frame.next {
                scan.diagnostics.push(Diagnostic::error(
                    line,
                    "LST001",
                    format!(
                        "ordered list item {number} follows {}",
                        frame.next.saturating_sub(1)
                    ),
                ));
            }
            frame.next = number.saturating_add(1);
            return;
        }
        scan.lists.truncate(index);
    } else if indent == 0 {
        scan.lists.clear();
    } else if scan
        .lists
        .last()
        .is_some_and(|frame| indent <= frame.indent)
        || scan.lists.is_empty()
    {
        let message = if scan.lists.is_empty() {
            "nested list item has no active parent list".to_owned()
        } else {
            format!("list indentation {indent} does not match an active level")
        };
        scan.diagnostics
            .push(Diagnostic::error(line, "LST002", message));
        return;
    }
    if kind == ListKind::Ordered && number != 1 {
        scan.diagnostics.push(Diagnostic::error(
            line,
            "LST001",
            format!("ordered list starts at {number}, expected 1"),
        ));
    }
    scan.lists.push(ListFrame {
        indent,
        kind,
        next: number.saturating_add(1),
    });
}

fn clause_candidate(text: &str) -> Option<(Result<Clause, &'static str>, &str)> {
    let mut words = text.trim_start().splitn(2, char::is_whitespace);
    let token = words.next()?;
    let first = token.chars().next()?;
    if first.is_ascii_digit()
        && (token.chars().all(|c| c.is_ascii_digit())
            || token.contains(['.', '(', ')'])
            || token.chars().any(|c| c.is_ascii_uppercase()))
    {
        Some((parse_clause(token), words.next().unwrap_or("").trim()))
    } else {
        None
    }
}

fn scan_document(document: &str) -> Scan {
    let mut scan = Scan {
        last_line: 1,
        ..Scan::default()
    };
    for (index, raw) in document.lines().enumerate() {
        let line = index + 1;
        scan.last_line = line;
        if let Some((marker, width)) = scan.fence {
            let text = raw.trim_start();
            let count = text.chars().take_while(|c| *c == marker).count();
            if count >= width && text[count..].trim().is_empty() {
                scan.fence = None;
            }
            continue;
        }
        if !scan.comment && raw.contains("<!--") && parse_heading(raw).is_some() {
            scan.diagnostics.push(Diagnostic::error(
                line,
                "HDR003",
                "heading contains an editor comment",
            ));
        }
        let (visible, comment) = strip_comments(scan.comment, raw);
        scan.comment = comment;
        if let Some(marker) = fence_marker(&visible) {
            scan.fence = Some(marker);
            scan.lists.clear();
            continue;
        }
        if visible.trim().is_empty() {
            continue;
        }
        scan.visible.push((line, visible.clone()));
        if let Some(heading) = parse_heading(&visible) {
            scan.lists.clear();
            match heading {
                Ok((level, text)) => scan.headings.push(Heading { line, level, text }),
                Err(message) => scan
                    .diagnostics
                    .push(Diagnostic::error(line, "HDR003", message)),
            }
        } else if let Some((indent, kind, number)) = parse_list(&visible) {
            update_list(&mut scan, line, indent, kind, number);
            if bold_clause_like(&visible) {
                scan.diagnostics.push(Diagnostic::error(
                    line,
                    "NUM006",
                    "bold list text may hide a clause label",
                ));
            }
            if kind == ListKind::Ordered {
                scan.ordered_items += 1;
                if indent == 0 && scan.first_top_ordered.is_none() {
                    scan.first_top_ordered = Some(line);
                }
            }
        } else if let Some((candidate, _body)) = clause_candidate(&visible) {
            scan.lists.clear();
            match candidate {
                Ok(clause) => scan.clauses.push((line, clause)),
                Err(message) => scan.diagnostics.push(Diagnostic::error(
                    line,
                    "NUM006",
                    format!("malformed clause label: {message}"),
                )),
            }
        } else {
            scan.lists.clear();
            if bold_clause_like(&visible) {
                scan.diagnostics.push(Diagnostic::error(
                    line,
                    "NUM006",
                    "bold text may hide a clause label",
                ));
            } else if visible.trim().len() >= 3 && visible.trim().bytes().all(|b| b == b'=') {
                scan.diagnostics.push(Diagnostic::error(
                    line,
                    "HDR003",
                    "Setext-style headings are unsupported; use ATX headings",
                ));
            } else if visible
                .trim()
                .strip_prefix('>')
                .is_some_and(|rest| rest.trim_start().starts_with(|c: char| c.is_ascii_digit()))
            {
                scan.diagnostics.push(Diagnostic::error(
                    line,
                    "NUM006",
                    "a block quote may hide a clause label",
                ));
            }
        }
    }
    if scan.fence.is_some() {
        scan.diagnostics.push(Diagnostic::error(
            scan.last_line,
            "HDR003",
            "unclosed fenced code block",
        ));
    }
    if scan.comment {
        scan.diagnostics.push(Diagnostic::error(
            scan.last_line,
            "HDR003",
            "unclosed HTML comment",
        ));
    }
    scan
}

fn sorted(mut findings: Vec<Diagnostic>) -> Vec<Diagnostic> {
    findings.sort();
    findings.dedup();
    findings
}

fn heading_diagnostics(headings: &[Heading]) -> Vec<Diagnostic> {
    let mut findings = Vec::new();
    let h1: Vec<_> = headings.iter().filter(|h| h.level == 1).collect();
    if h1.len() != 1 {
        findings.push(Diagnostic::error(
            h1.first().map_or(1, |h| h.line),
            "HDR001",
            format!("expected exactly one H1 document title, found {}", h1.len()),
        ));
    }
    match headings.first() {
        None => findings.push(Diagnostic::error(
            1,
            "HDR001",
            "the document contains no headings",
        )),
        Some(h) if h.level != 1 => findings.push(Diagnostic::error(
            h.line,
            "HDR001",
            "the first substantive heading is not the document H1",
        )),
        _ => {}
    }
    let mut seen = HashMap::new();
    for (index, heading) in headings.iter().enumerate() {
        if index > 0 && heading.level > headings[index - 1].level + 1 {
            findings.push(Diagnostic::error(
                heading.line,
                "HDR002",
                format!(
                    "heading level jumps from H{} to H{}",
                    headings[index - 1].level,
                    heading.level
                ),
            ));
        }
        let key = heading
            .text
            .chars()
            .map(|c| {
                if c.is_alphanumeric() {
                    c.to_lowercase().to_string()
                } else {
                    " ".into()
                }
            })
            .collect::<String>()
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ");
        if let Some(first) = seen.get(&key) {
            findings.push(Diagnostic::error(
                heading.line,
                "HDR003",
                format!("heading duplicates line {first}: {}", heading.text),
            ));
        } else {
            seen.insert(key, heading.line);
        }
    }
    findings
}

fn initial_sibling(parent: Option<&Clause>, clause: &Clause) -> bool {
    match (parent, clause.0.as_slice().last()) {
        (None, Some(Part::Decimal(1))) if clause.0.len() == 1 => true,
        (Some(_), Some(Part::Decimal(1) | Part::Roman(1) | Part::Alpha('a'))) => true,
        _ => false,
    }
}

fn valid_successor(previous: &Clause, current: &Clause) -> bool {
    match (previous.0.as_slice(), current.0.as_slice()) {
        ([Part::Decimal(a)], [Part::Decimal(b)]) => *b == a.saturating_add(1),
        ([Part::Decimal(a)], [Part::Decimal(b), Part::Alpha('A')]) => a == b,
        ([Part::Decimal(a), Part::Alpha(x)], [Part::Decimal(b), Part::Alpha(y)])
            if x.is_ascii_uppercase() && y.is_ascii_uppercase() =>
        {
            a == b && (*y as u32) == (*x as u32 + 1)
        }
        ([Part::Decimal(a), Part::Alpha(x)], [Part::Decimal(b)]) if x.is_ascii_uppercase() => {
            *b == a.saturating_add(1)
        }
        (left, right)
            if left.len() == right.len()
                && left.len() > 1
                && left[..left.len() - 1] == right[..right.len() - 1] =>
        {
            match (left.last(), right.last()) {
                (Some(Part::Decimal(a)), Some(Part::Decimal(b)))
                | (Some(Part::Roman(a)), Some(Part::Roman(b))) => *b == a.saturating_add(1),
                (Some(Part::Alpha(a)), Some(Part::Alpha(b))) => (*b as u32) == (*a as u32 + 1),
                _ => false,
            }
        }
        _ => false,
    }
}

fn structure_diagnostics(scan: &Scan, strict: bool) -> Vec<Diagnostic> {
    let mut findings = scan.diagnostics.clone();
    findings.extend(heading_diagnostics(&scan.headings));
    if scan.clauses.is_empty() {
        if strict {
            findings.push(Diagnostic::error(
                1,
                "NUM001",
                "strict validation recognised zero clauses",
            ));
        } else {
            findings.push(Diagnostic::warning(1, "NUM000", "canonical clause numbering is not present; validated headings and list hierarchy only (0 explicit clauses; strict reference validation is limited)"));
        }
    }
    let mut seen = HashMap::new();
    let mut declared = HashSet::new();
    let mut previous_by_parent = HashMap::<Option<Clause>, Clause>::new();
    for (index, (line, clause)) in scan.clauses.iter().enumerate() {
        if let Some(first) = seen.insert(clause, line) {
            findings.push(Diagnostic::error(
                *line,
                "NUM002",
                format!(
                    "duplicate clause {} (first declared on line {first})",
                    clause.render()
                ),
            ));
        }
        let parent = clause.parent();
        if parent.as_ref().is_some_and(|p| !declared.contains(p)) {
            findings.push(Diagnostic::error(
                *line,
                "NUM003",
                format!(
                    "clause {} has missing parent {}",
                    clause.render(),
                    parent.as_ref().unwrap().render()
                ),
            ));
        }
        declared.insert(clause.clone());
        if let Some(previous) = previous_by_parent.insert(parent.clone(), clause.clone()) {
            if !valid_successor(&previous, clause) {
                findings.push(Diagnostic::error(
                    *line,
                    "NUM004",
                    format!(
                        "clause {} does not follow {}",
                        clause.render(),
                        previous.render()
                    ),
                ));
            }
        } else if !initial_sibling(parent.as_ref(), clause) {
            findings.push(Diagnostic::error(
                *line,
                "NUM004",
                format!(
                    "first sibling clause is not the initial identifier: {}",
                    clause.render()
                ),
            ));
        }
        if index > 0 && clause.depth() > scan.clauses[index - 1].1.depth() + 1 {
            findings.push(Diagnostic::error(
                *line,
                "NUM005",
                format!(
                    "clause {} skips a nesting level after {}",
                    clause.render(),
                    scan.clauses[index - 1].1.render()
                ),
            ));
        }
    }
    if !scan.clauses.is_empty() {
        if let Some(line) = scan.first_top_ordered {
            findings.push(Diagnostic::error(
                line,
                "NUM007",
                "explicit clauses are mixed with top-level ordered-list numbering",
            ));
        }
    }
    sorted(findings)
}

fn strip_token(text: &str) -> &str {
    text.trim_matches(|c| ",.;:".contains(c))
}

fn relative_reference(
    kind: &str,
    target: &str,
    current: Option<&Clause>,
) -> Result<Option<Clause>, &'static str> {
    let inside = target
        .strip_prefix('(')
        .and_then(|text| text.strip_suffix(')'))
        .ok_or("malformed relative reference")?;
    let (depth, part) = match kind {
        "subsection" => (2, Part::Decimal(parse_decimal(inside)?)),
        "paragraph" if inside.len() == 1 && inside.bytes().all(|b| b.is_ascii_lowercase()) => {
            (3, Part::Alpha(inside.chars().next().unwrap()))
        }
        "subparagraph" => (4, Part::Roman(parse_roman(inside)?)),
        _ => return Err("unexpected relative reference level"),
    };
    let Some(current) = current else {
        return Ok(None);
    };
    let prefix_depth = depth - 1;
    if current.depth() < prefix_depth {
        return Ok(None);
    }
    let inserted =
        usize::from(matches!(current.0.get(1), Some(Part::Alpha(c)) if c.is_ascii_uppercase()));
    let mut parts = current.0[..prefix_depth + inserted].to_vec();
    parts.push(part);
    Ok(Some(Clause(parts)))
}

fn reference_diagnostics(scan: &Scan) -> Vec<Diagnostic> {
    let clauses: HashSet<_> = scan.clauses.iter().map(|(_, clause)| clause).collect();
    let numbered = !clauses.is_empty();
    let mut findings = Vec::new();
    for (line, text) in &scan.visible {
        if parse_heading(text).is_some() {
            continue;
        }
        let tokens: Vec<_> = text.split_whitespace().collect();
        for (index, token) in tokens.iter().enumerate() {
            let kind = strip_token(token).to_lowercase();
            if ![
                "section",
                "clause",
                "clauses",
                "paragraph",
                "subparagraph",
                "subsection",
            ]
            .contains(&kind.as_str())
            {
                continue;
            }
            let mut targets = Vec::new();
            if let Some(target) = tokens.get(index + 1) {
                targets.push(*target);
            }
            if kind == "clauses" {
                let mut cursor = index + 2;
                while cursor + 1 < tokens.len()
                    && ["and", "&"].contains(&strip_token(tokens[cursor]).to_lowercase().as_str())
                {
                    targets.push(tokens[cursor + 1]);
                    cursor += 2;
                }
            }
            for (target_index, raw) in targets.iter().enumerate() {
                let target = strip_token(raw);
                if !target.starts_with(|c: char| c.is_ascii_digit() || c == '(') {
                    continue;
                }
                let target_kind = if target_index == 0 {
                    kind.as_str()
                } else {
                    "clause"
                };
                let current = scan
                    .clauses
                    .iter()
                    .rev()
                    .find(|(at, _)| at <= line)
                    .map(|(_, clause)| clause);
                let parsed = if target.starts_with('(') {
                    relative_reference(target_kind, target, current)
                } else {
                    parse_clause(target).map(Some)
                };
                match parsed {
                    Err(_) => findings.push(Diagnostic { line: *line, severity: if numbered { Severity::Error } else { Severity::Warning }, code: "REF002", message: format!("malformed {target_kind} reference '{target}'") }),
                    Ok(None) => findings.push(Diagnostic { line: *line, severity: if numbered { Severity::Error } else { Severity::Warning }, code: "REF001", message: format!("cannot resolve relative {target_kind} reference {target} without an enclosing clause") }),
                    Ok(Some(clause)) if numbered && !clauses.contains(&clause) => findings.push(Diagnostic::error(*line, "REF001", format!("unresolved {target_kind} reference {}", clause.render()))),
                    Ok(Some(clause)) if numbered && !reference_level_matches(target_kind, clause.depth()) => findings.push(Diagnostic::error(*line, "REF003", format!("{target_kind} reference has the wrong identifier depth: {}", clause.render()))),
                    Ok(Some(clause)) if !numbered => findings.push(Diagnostic::warning(*line, "REF001", format!("cannot resolve {target_kind} reference {} in the legacy unnumbered document", clause.render()))),
                    _ => {}
                }
            }
        }
    }
    findings
}

fn reference_level_matches(kind: &str, depth: usize) -> bool {
    match kind {
        "section" => depth == 1,
        "clause" | "subsection" => depth == 2,
        "paragraph" => depth == 3,
        "subparagraph" => depth == 4,
        _ => true,
    }
}

fn role_forms<'a>(phrase: &str, source: &'a str) -> Vec<&'a str> {
    let mut result = Vec::new();
    let lowercase = source.to_ascii_lowercase();
    let variants: &[&str] = if phrase == "vice-president" {
        &["vice-president", "vice president"]
    } else {
        &[phrase]
    };
    for wanted in variants {
        for (start, _) in lowercase.match_indices(wanted) {
            let end = start + wanted.len();
            if source[..start]
                .chars()
                .next_back()
                .is_some_and(char::is_alphanumeric)
                || source[end..]
                    .chars()
                    .next()
                    .is_some_and(char::is_alphanumeric)
            {
                continue;
            }
            result.push((start, &source[start..end]));
        }
    }
    result.sort_by_key(|(start, _)| *start);
    result.into_iter().map(|(_, form)| form).collect()
}

fn prose_text(scan: &Scan) -> String {
    scan.visible
        .iter()
        .filter(|(_, text)| parse_heading(text).is_none())
        .map(|(_, text)| text.as_str())
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_lowercase()
}

fn role_diagnostics(scan: &Scan, all_prose: &str) -> Vec<Diagnostic> {
    let prose: Vec<_> = scan
        .visible
        .iter()
        .filter(|(_, text)| parse_heading(text).is_none())
        .collect();
    let composition = prose
        .iter()
        .filter(|(_, text)| {
            let lower = text.to_ascii_lowercase();
            [
                "consist of",
                "comprise",
                "composed of",
                "include the following",
            ]
            .iter()
            .any(|phrase| lower.contains(phrase))
        })
        .map(|(_, text)| text.as_str())
        .collect::<Vec<_>>()
        .join(" ")
        .to_ascii_lowercase();
    let offices = [
        (
            "President",
            "president",
            !role_forms("president", &composition).is_empty(),
        ),
        (
            "Secretary",
            "secretary",
            !role_forms("secretary", &composition).is_empty(),
        ),
        (
            "Treasurer",
            "treasurer",
            !role_forms("treasurer", &composition).is_empty(),
        ),
        (
            "Vice-President",
            "vice-president",
            !role_forms("vice-president", &composition).is_empty(),
        ),
        (
            "Returning Officer",
            "returning officer",
            [
                "select a returning officer",
                "elect a returning officer",
                "appoint a returning officer",
            ]
            .iter()
            .any(|phrase| all_prose.contains(phrase)),
        ),
    ];
    let mut findings = Vec::new();
    for (display, phrase, established) in offices {
        if !established {
            if let Some((line, _)) = prose
                .iter()
                .find(|(_, text)| !role_forms(phrase, text).is_empty())
            {
                findings.push(Diagnostic::warning(
                    *line,
                    "ROLE001",
                    format!("{display} is used but not established"),
                ));
            }
        }
    }
    for phrase in [
        "president",
        "secretary",
        "treasurer",
        "vice-president",
        "returning officer",
        "management committee",
        "governing committee",
        "executive committee",
        "exec committee",
        "annual general meeting",
        "special general meeting",
    ] {
        let mut first = None;
        'lines: for (line, text) in &prose {
            for form in role_forms(phrase, text) {
                match &first {
                    None => first = Some(form),
                    Some(initial) if !initial.eq_ignore_ascii_case(form) => {
                        findings.push(Diagnostic::warning(
                            *line,
                            "ROLE002",
                            format!("inconsistent role form '{form}'; first used as '{initial}'"),
                        ));
                        break 'lines;
                    }
                    _ => {}
                }
            }
        }
    }
    findings
}

fn written_number(words: impl IntoIterator<Item = impl AsRef<str>>) -> Option<u64> {
    let mut total = 0_u64;
    let mut current = 0_u64;
    for word in words {
        let value = match word.as_ref().to_lowercase().as_str() {
            "zero" => 0,
            "one" => 1,
            "two" => 2,
            "three" => 3,
            "four" => 4,
            "five" => 5,
            "six" => 6,
            "seven" => 7,
            "eight" => 8,
            "nine" => 9,
            "ten" => 10,
            "eleven" => 11,
            "twelve" => 12,
            "thirteen" => 13,
            "fourteen" => 14,
            "fifteen" => 15,
            "sixteen" => 16,
            "seventeen" => 17,
            "eighteen" => 18,
            "nineteen" => 19,
            "twenty" => 20,
            "thirty" => 30,
            "forty" => 40,
            "fifty" => 50,
            "sixty" => 60,
            "seventy" => 70,
            "eighty" => 80,
            "ninety" => 90,
            "hundred" => {
                current = current.max(1).checked_mul(100)?;
                continue;
            }
            "thousand" => {
                total = total.checked_add(current.max(1).checked_mul(1000)?)?;
                current = 0;
                continue;
            }
            _ => return None,
        };
        current = current.checked_add(value)?;
    }
    total.checked_add(current)
}

fn clean_word(text: &str) -> String {
    text.chars()
        .filter(|c| c.is_alphanumeric() || *c == '-')
        .collect::<String>()
        .to_lowercase()
}

fn compare_pair(before: &str, inside: &str) -> Option<(String, bool)> {
    if inside.contains('/') {
        let word = clean_word(before.split_whitespace().last()?);
        let (top, bottom) = word.split_once('-')?;
        let denominator = match bottom {
            "half" | "halves" => 2,
            "third" | "thirds" => 3,
            "quarter" | "quarters" => 4,
            _ => return None,
        };
        let (numerator, divisor) = inside.split_once('/')?;
        let matches = (written_number([top])?, denominator)
            == (numerator.parse().ok()?, divisor.parse().ok()?);
        return Some((word, matches));
    }
    if inside.starts_with('$') {
        let words: Vec<_> = before.split_whitespace().collect();
        if !["dollar", "dollars"].contains(&strip_token(words.last()?).to_lowercase().as_str()) {
            return None;
        }
        let written = words[..words.len() - 1]
            .iter()
            .rev()
            .take(3)
            .rev()
            .map(|word| clean_word(word))
            .collect::<Vec<_>>()
            .join(" ");
        let number = written_number(written.split_whitespace())?;
        let numeral = inside
            .chars()
            .filter(char::is_ascii_digit)
            .collect::<String>()
            .parse::<u64>()
            .ok()?;
        return Some((written, number == numeral));
    }
    if !inside.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let word = clean_word(before.split_whitespace().last()?);
    let number = written_number([word.as_str()])?;
    Some((word, number == inside.parse::<u64>().ok()?))
}

fn number_diagnostics(scan: &Scan) -> Vec<Diagnostic> {
    let mut findings = Vec::new();
    for (line, source) in &scan.visible {
        let mut rest = source.as_str();
        while let Some((before, after_open)) = rest.split_once('(') {
            let Some((inside, after_close)) = after_open.split_once(')') else {
                break;
            };
            if let Some((written, false)) = compare_pair(before, inside) {
                findings.push(Diagnostic::error(
                    *line,
                    "TEXT001",
                    format!("written number and numeral disagree: {written} ({inside})"),
                ));
            }
            rest = after_close;
        }
    }
    findings
}

fn draft_diagnostics(document: &str) -> Vec<Diagnostic> {
    document
        .lines()
        .enumerate()
        .filter_map(|(index, line)| {
            let lower = line.to_lowercase();
            let message = if ["<<<<<<<", "=======", ">>>>>>>"]
                .iter()
                .any(|mark| line.starts_with(mark))
            {
                "merge-conflict marker remains in the document"
            } else if lower
                .split(|c: char| !c.is_alphanumeric())
                .any(|word| ["todo", "fixme", "tbd"].contains(&word))
            {
                "unresolved editor marker remains in the document"
            } else if line.contains("<!--")
                && ["should", "editor", "resolve", "placeholder"]
                    .iter()
                    .any(|word| lower.contains(word))
            {
                "unresolved editor comment remains in the document"
            } else {
                return None;
            };
            Some(Diagnostic::error(index + 1, "DRAFT001", message))
        })
        .collect()
}

fn governance_diagnostics(scan: &Scan, provisions: &str) -> Vec<Diagnostic> {
    let mut findings = Vec::new();
    let topics: &[(&str, &[&str])] = &[
        ("membership", &["membership", "admitted as a member"]),
        (
            "committee governance",
            &[
                "management committee",
                "governing committee",
                "executive committee",
                "exec committee",
            ],
        ),
        ("general meetings", &["general meeting"]),
        ("quorum", &["quorum"]),
        ("meeting notice", &["notice", "notify"]),
        (
            "rule changes",
            &["amend", "alteration", "change these rules"],
        ),
        ("dissolution", &["dissolv", "winding up"]),
    ];
    for (topic, names) in topics {
        if !names.iter().any(|name| provisions.contains(name)) {
            findings.push(Diagnostic::error(
                1,
                "GOV001",
                format!("constitution has no operative text addressing {topic}"),
            ));
        }
    }
    for (index, heading) in scan.headings.iter().enumerate() {
        let end = scan.headings[index + 1..]
            .iter()
            .find(|next| next.level <= heading.level)
            .map_or(usize::MAX, |next| next.line);
        if !scan.visible.iter().any(|(line, text)| {
            *line > heading.line
                && *line < end
                && parse_heading(text).is_none()
                && text.chars().any(char::is_alphanumeric)
        }) {
            findings.push(Diagnostic::error(
                heading.line,
                "GOV002",
                format!("section '{}' has no operative text", heading.text),
            ));
        }
    }
    findings
}

fn integrity_diagnostics(scan: &Scan, document: &str) -> Vec<Diagnostic> {
    let provisions = prose_text(scan);
    let mut findings = Vec::new();
    findings.extend(reference_diagnostics(scan));
    findings.extend(role_diagnostics(scan, &provisions));
    findings.extend(number_diagnostics(scan));
    findings.extend(draft_diagnostics(document));
    findings.extend(governance_diagnostics(scan, &provisions));
    sorted(findings)
}

fn drafting_diagnostics(scan: &Scan) -> Vec<Diagnostic> {
    let rules = [
        ("50% of members plus one", "LAW001", "State the required whole-number threshold and how it is calculated"),
        ("50% plus one", "LAW001", "State the required whole-number threshold and how it is calculated"),
        ("and/or", "LAW002", "State whether both, either or one alternative is intended"),
        ("he or she", "LAW003", "Use a gender-neutral reference if the rule applies to everyone"),
        ("himself or herself", "LAW003", "Use a gender-neutral reference if the rule applies to everyone"),
        ("his or her", "LAW003", "Use a gender-neutral reference if the rule applies to everyone"),
        ("shall be at liberty to", "LAW004", "Consider whether 'may' confers this power without changing its legal effect"),
        ("subject as previously provided", "LAW005", "Identify the provision that qualifies this rule"),
        ("aforestated", "LAW005", "Name the officeholders or provision referred to"),
        ("injurious or prejudicial", "LAW006", "Define the conduct or decision criterion this standard covers"),
        ("full and fair opportunity", "LAW006", "Specify notice and opportunity to respond if those procedural rights are intended"),
        ("at its sole discretion", "LAW006", "Identify any conditions or limits on the discretion"),
        ("at their sole discretion", "LAW006", "Identify any conditions or limits on the discretion"),
        ("as deemed necessary", "LAW006", "Identify who decides and on what criterion"),
        ("as soon as practicable", "LAW007", "Confirm that an open time standard is intended; state a period or trigger if certainty is required"),
        ("without undue delay", "LAW007", "Confirm that an open time standard is intended; state a period or trigger if certainty is required"),
        ("within a reasonable time", "LAW007", "Confirm that an open time standard is intended; state a period or trigger if certainty is required"),
    ];
    let mut findings = Vec::new();
    for (line, text) in &scan.visible {
        if parse_heading(text).is_some() {
            continue;
        }
        for (phrase, code, reason) in rules {
            if let Some(found) = role_forms(phrase, text).first() {
                findings.push(Diagnostic::warning(
                    *line,
                    code,
                    format!("{found}: {reason}"),
                ));
            }
        }
    }
    sorted(findings)
}

fn emit(findings: &[Diagnostic]) -> bool {
    for finding in findings {
        let severity = if finding.severity == Severity::Error {
            "ERROR"
        } else {
            "WARN"
        };
        let message = finding.message.replace(['\t', '\r', '\n'], " ");
        println!("{severity}\t{}\t{}\t{message}", finding.code, finding.line);
    }
    !findings
        .iter()
        .any(|finding| finding.severity == Severity::Error)
}

fn read_document(path: &str) -> Result<String, String> {
    if fs::metadata(path)
        .map_err(|error| format!("cannot read {path}: {error}"))?
        .len()
        > 1_048_576
    {
        return Err(format!("{path} exceeds the 1 MiB document limit"));
    }
    let document =
        fs::read_to_string(path).map_err(|error| format!("cannot read {path}: {error}"))?;
    if document.is_empty() {
        Err(format!("{path} is empty"))
    } else {
        Ok(document)
    }
}

fn run(args: &[String]) -> Result<bool, String> {
    match args {
        [command] if command == "selfcheck" => Ok(selfcheck()),
        [command, path] if command == "structure" => {
            let scan = scan_document(&read_document(path)?);
            let passed = emit(&structure_diagnostics(&scan, false));
            println!(
                "STAT\tSTRUCTURE\t0\theadings={};clauses={};ordered-items={}",
                scan.headings.len(),
                scan.clauses.len(),
                scan.ordered_items
            );
            Ok(passed)
        }
        [command, path] if command == "integrity" => {
            let document = read_document(path)?;
            let scan = scan_document(&document);
            let findings = integrity_diagnostics(&scan, &document);
            let passed = emit(&findings);
            let references = findings
                .iter()
                .filter(|finding| ["REF001", "REF002", "REF003"].contains(&finding.code))
                .count();
            println!(
                "STAT\tINTEGRITY\t0\tclauses={};reference-findings={references}",
                scan.clauses.len()
            );
            Ok(passed)
        }
        [command, path] if command == "drafting" => {
            let scan = scan_document(&read_document(path)?);
            Ok(emit(&drafting_diagnostics(&scan)))
        }
        _ => Err(
            "usage: structure selfcheck | structure structure|drafting|integrity Constitution.md"
                .into(),
        ),
    }
}

fn selfcheck() -> bool {
    let valid = "# Title\n\n## Section\n\n### Detail\n";
    let strict =
        "# Title\n\n1 First\n1.1 Child\n1.1(a) Paragraph\n1.1(a)(i) Subparagraph\n2 Second\n";
    let code = |source: &str, strict: bool, wanted: &str| {
        structure_diagnostics(&scan_document(source), strict)
            .iter()
            .any(|finding| finding.code == wanted)
    };
    let integrity_code = |source: &str, wanted: &str| {
        integrity_diagnostics(&scan_document(source), source)
            .iter()
            .any(|finding| finding.code == wanted)
    };
    let governance_topic = |source: &str, topic: &str| {
        let scan = scan_document(source);
        governance_diagnostics(&scan, &prose_text(&scan))
            .iter()
            .any(|finding| finding.code == "GOV001" && finding.message.ends_with(topic))
    };
    let legal_findings = drafting_diagnostics(&scan_document("# Title\nA quorum is 50% plus one and/or appointed members.\nA member may present himself or herself.\nThe officer shall be at liberty to act.\nSubject as previously provided, the committee may meet.\nConduct injurious or prejudicial to the club may be considered.\nReport within a reasonable time.\n"));
    let cases = [
        ("valid heading hierarchy", !structure_diagnostics(&scan_document(valid), false).iter().any(|f| f.severity == Severity::Error)),
        ("heading jump", code("# Title\n### Jump\n", false, "HDR002")),
        ("valid strict numbering", !structure_diagnostics(&scan_document(strict), true).iter().any(|f| f.severity == Severity::Error)),
        ("strict zero clauses", code(valid, true, "NUM001")),
        ("duplicate clause", code("# Title\n1 First\n1 First again\n", true, "NUM002")),
        ("missing parent", code("# Title\n1.1 Orphan\n", true, "NUM003")),
        ("skipped sibling", code("# Title\n1 First\n3 Third\n", true, "NUM004")),
        ("mixed numbering", code("# Title\n1 First\n\n1. List item\n", true, "NUM007")),
        ("reserved clause", !structure_diagnostics(&scan_document("# Title\n1 [Reserved]\n"), true).iter().any(|f| f.severity == Severity::Error)),
        ("repealed clause", !structure_diagnostics(&scan_document("# Title\n1 [Repealed]\n"), true).iter().any(|f| f.severity == Severity::Error)),
        ("unresolved reference", integrity_code("# Title\n1 First\n\nSee section 2.\n", "REF001")),
        ("relative references resolve", !reference_diagnostics(&scan_document("# Title\n1 First\n1.1 One\n1.2 Two\n1.2(a) Alpha\n1.2(a)(i) Roman\nSee subsection (2), paragraph (a) and subparagraph (i).\n")).iter().any(|f| f.code.starts_with("REF"))),
        ("relative references preserve inserted sections", !reference_diagnostics(&scan_document("# Title\n1 First\n1A Inserted\n1A.1 One\n1A.2 Two\nSee subsection (2).\n")).iter().any(|f| f.code.starts_with("REF"))),
        ("missing relative subsection", integrity_code("# Title\n1 First\n1.1 One\nSee subsection (2).\n", "REF001")),
        ("legacy relative subsection", reference_diagnostics(&scan_document("# Title\nSee subsection (2).\n")).iter().any(|f| f.code == "REF001") && !reference_diagnostics(&scan_document("# Title\nSee subsection (2).\n")).iter().any(|f| f.code == "REF002")),
        ("malformed relative subsection", integrity_code("# Title\n1 First\nSee subsection (x).\n", "REF002")),
        ("undefined office", integrity_code("# Title\n## Governance\nThe committee shall consist of a president, secretary, and treasurer.\n## Meetings\nThe vice-president chairs.\n", "ROLE001")),
        ("office definition survives heading changes", !integrity_code("# Title\n## Governance\nThe committee shall consist of a president, secretary, and treasurer.\n", "ROLE001")),
        ("case variation is not a role change", !integrity_code("# Title\nThe annual general meeting is held. At the Annual General Meeting, members vote.\n", "ROLE002")),
        ("role spelling variation", integrity_code("# Title\nA vice-president may preside. The vice president may vote.\n", "ROLE002")),
        ("matching number", !integrity_code("# Title\nFourteen (14) days.\n", "TEXT001")),
        ("mismatching number", integrity_code("# Title\nFourteen (13) days.\n", "TEXT001")),
        ("inserted section", parse_clause("8A.1") == Ok(Clause(vec![Part::Decimal(8), Part::Alpha('A'), Part::Decimal(1)]))),
        ("hidden clause", code("# Title\n**1a.** Text\n", false, "NUM006")),
        ("unrecognised strict document", structure_diagnostics(&scan_document("plain text only\n"), true).iter().any(|f| f.severity == Severity::Error)),
        ("missing governance topic", integrity_code("# Title\n## Membership\nMembers may join.\n", "GOV001")),
        ("heading alone is not a membership rule", governance_topic("# Title\n## Membership\nRules apply.\n", "membership")),
        ("renamed membership heading", !governance_topic("# Title\n## Participation\nMembership begins on admission.\n", "membership")),
        ("exec committee is recognised", !governance_topic("# Title\nThe exec committee manages affairs.\n", "committee governance")),
        ("unlisted board title is not assumed", governance_topic("# Title\nThe board of directors manages affairs.\n", "committee governance")),
        ("drafting decision rule", drafting_diagnostics(&scan_document("# Title\nA quorum is 50% of members plus one, elected and/or appointed.\n")).iter().filter(|finding| ["LAW001", "LAW002"].contains(&finding.code)).count() == 2),
        ("drafting categories", ["LAW001", "LAW002", "LAW003", "LAW004", "LAW005", "LAW006", "LAW007"].iter().all(|code| legal_findings.iter().any(|finding| finding.code == *code))),
        ("established legal term retained", drafting_diagnostics(&scan_document("# Title\nThe members may from time to time set the fee.\n")).is_empty()),
        ("empty section", integrity_code("# Title\n## Membership\n## Dissolution\nRules apply.\n", "GOV002")),
        ("editor marker needs a word boundary", draft_diagnostics("Todorov may vote.\n").is_empty()),
        ("TBD is unresolved", draft_diagnostics("The quorum is TBD.\n").iter().any(|f| f.code == "DRAFT001")),
    ];
    let failures: Vec<_> = cases
        .iter()
        .filter(|(_, passed)| !passed)
        .map(|(name, _)| *name)
        .collect();
    for name in &failures {
        println!("Self-check failed: {name}");
    }
    println!(
        "Self-check: {} passed, {} failed",
        cases.len() - failures.len(),
        failures.len()
    );
    failures.is_empty()
}

fn main() -> ExitCode {
    let args: Vec<_> = env::args().skip(1).collect();
    match run(&args) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(2)
        }
    }
}
