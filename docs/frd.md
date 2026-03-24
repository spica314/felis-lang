# Functional Requirements Document

This repository manages primary functional requirements in
[`frd.json`](../frd.json). See also [`docs/README.md`](./README.md).

## Purpose

[`frd.json`](../frd.json) is the source of truth for the functional
requirements document.

- The format is JSON so it can be processed mechanically with tools such as
  `jq`.
- The file should remain a reliable source of information for both humans and
  AI-assisted workflows.
- The file describes required externally observable behavior rather than the
  current implementation status.

## Structure

The top-level structure is:

```json
{
  "requirements": []
}
```

Repository-level materials related to the functional requirements are organized
as follows:

- [`frd.json`](../frd.json): machine-processable source of truth for
  requirements.
- [`docs/`](./): explanatory documents and design notes.
- [`std/`](../std/): reserved location for standard library source code, kept
  separate from requirements and design documentation.

Each requirement object uses the following members:

- `id`: Functional requirement ID. Must start with `FR-`.
- `title`: Short human-readable title.
- `description`: The requirement itself.
- `notes`: Optional supplementary notes.
- `children`: Optional child requirements.
- `acceptanceCriteria`: Acceptance criteria. Only allowed on leaf requirements.
- `docs`: Optional list of Markdown documents for detailed explanations.

Example:

```json
{
  "requirements": [
    {
      "id": "FR-001",
      "title": "Algebraic Type",
      "description": "The language supports algebraic data types.",
      "notes": "",
      "children": [
        {
          "id": "FR-001-001",
          "title": "Syntax",
          "description": "Users can declare algebraic data types in source code.",
          "acceptanceCriteria": [
            {
              "description": "A valid declaration is accepted by the compiler."
            }
          ],
          "notes": "",
          "docs": [
            "docs/algebraic-type.md"
          ]
        }
      ]
    }
  ]
}
```

## Authoring Rules

- IDs use `FR-` followed by three-digit segments per hierarchy level.
  Example: `FR-001`, `FR-001-001`, `FR-001-001-001`.
- Use hierarchy to break requirements down into smaller units.
- Prefer leaf requirements whose `acceptanceCriteria` converge on a single
  observable behavior.
- Only leaf requirements may define `acceptanceCriteria`.
- Behaviors observable from outside the system should be captured as
  requirements when they matter for compatibility.
- Do not encode transient implementation state such as pass/fail information in
  `frd.json`.
- Put detailed explanatory material in Markdown files and reference them from
  `docs`.

## Maintenance

- Keep [`frd.json`](../frd.json) small and structured.
- Put long-form rationale or design notes in [`docs/`](./).
- When adding a requirement, update referenced Markdown files in the same
  change.
