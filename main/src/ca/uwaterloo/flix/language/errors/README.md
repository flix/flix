# Error Message Guidelines

Best practices for writing clear, helpful, and consistent error messages in the Flix compiler.

---

## Goals

A good error message should:

- Help the programmer quickly understand **what went wrong**
- Explain **why it happened** in approachable terms
- Provide **clear next steps** to resolve the issue

Error messages are part of the user experience. Treat them as documentation that appears exactly when it is needed.

---

## Structure

Every error consists of three components, each with a distinct purpose:

1. **Summary**  
   A single, concise sentence shown on hover in VS Code. It should be understandable in isolation.

2. **Message**  
   Multi-line content that highlights relevant symbols, identifiers, and code fragments.

3. **Explanation**  
   A brief explanation of why the problem occurs and how to fix it.

Avoid blurring the responsibilities of these components. Each should contribute new information.

---

## Style and Tone

- Be **crisp, concise, and precise**
- Use **friendly, neutral language**—never blame the programmer
- Prefer neutral wording such as **“unexpected”** over judgmental terms like “illegal”

### Straight to the Point

Lead with the error type or category to support quick scanning.

**Good:**  
`Duplicate definition: 'foo'`

**Less effective:**  
`The definition 'foo' is defined twice`

The first word should immediately orient the reader.

---

### Be Actionable

Every error should answer the implicit question: *“What should I do next?”*

Include concrete guidance such as:

- Which construct to change
- Which keyword to use instead
- Where a definition is expected to appear

---

### Consistent Error Shapes

Similar errors should look similar.

- Use the same summary patterns
- Use the same labels
- Present information in the same order

Users learn how to read error messages through pattern recognition.

**Example:**
- `Undefined variable: 'x'`
- `Undefined effect: 'IO'`
- `Undefined type: 'Foo'`

---

### Avoid Over-Explaining Obvious Errors

Not every error needs a tutorial.

- Simple syntax mistakes → brief explanation and fix
- Advanced typing or effect errors → deeper explanation

Treat explanations as **progressively disclosed**, not mandatory. Overly verbose messages for common mistakes slow users down.

---

## Formatting

### Identifiers

- User-written identifiers must be quoted with **single quotes**
- Apply color consistently based on semantic role

### Colors

- `red()` — problematic or rejected code
- `cyan()` — expected values or shapes
- `magenta()` — names, symbols, and identifiers

Use color to guide attention, not to decorate.

---

### Labels

Use labels consistently to signal intent:

- **`Tip:`** — a single-line, practical hint
- **`Explanation:`** — multi-line educational content
- **`Possible fixes:`** — a list of concrete alternatives

Avoid inventing new labels unless they serve a clearly distinct purpose.

---

### Source Hints

The `src(loc, hint)` annotation should:

- Add *contextual* information
- Highlight the relevant code region
- Avoid restating the summary or message lead

A good source hint helps the user understand *why this location matters*.
