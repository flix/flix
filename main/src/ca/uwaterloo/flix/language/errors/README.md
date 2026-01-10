# Error Message Guidelines

Best practices for writing error messages in the Flix compiler.

## The 80/20 Rule

80% of the time developers need minimal info (they've seen the error before). 20% of the time they need full context (it's new to them). Messages should accommodate both.

## Structure

Every error has three components:

1. **Summary** - One sentence shown on hover in VS Code
2. **Message** - Multi-line text with relevant symbols and code fragments
3. **Explanation** - Why the problem occurs and how to fix it

## Style and Tone

Messages should be crisp, concise, and clear. Language should be friendly, never blame the programmer. Prefer "unexpected" over "illegal".

### Straight to the Point

Lead with the error type. "Duplicate definition: 'foo'" beats "The definition 'foo' is defined twice" - the programmer only has to scan the first word.

## Key Principles

### 1. Don't Repeat Yourself
Each component (summary, message lead, src hint) should add new information.

### 2. Show What's Allowed
When rejecting something, explain what is valid in that context.

### 3. Explain Technical Concepts
Don't assume users know type theory, Datalog, or compiler internals.

### 4. Be Actionable
Include concrete guidance on how to fix the error.

### 5. Suggest Alternatives
When applicable, suggest "did you mean?" for typos or list available options.

## Formatting

### Identifiers

User-written identifiers should be quoted with single quotes and colored (color depends on context).

### Colors
- `red()` - Problematic code
- `cyan()` - Expected values
- `magenta()` - Names and symbols

### Labels
- `Tip:` - Single-line advice
- `Explanation:` - Multi-line educational content
- `Possible fixes:` - List of alternatives

### Source Hints
The `src(loc, hint)` hint should provide context, not echo the summary.
