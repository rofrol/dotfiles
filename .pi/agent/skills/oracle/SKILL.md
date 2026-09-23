---
name: oracle
description: Ask an independent read-only reviewer model (GPT Astra) for a second opinion on a solution, claim, or diff via pi-fabric in a visible Herdr tab. Use when the user runs /skill:oracle or asks for an oracle/independent review.
---

# Oracle

Run one independent, read-only reviewer as a pi-fabric child in a Herdr tab and
return its attributable result.

Supported reviewer: `astra` (default). Reject any other name; do not invent
model keys.

## 1. Build the review package

The reviewer does NOT see this conversation. Write two parts:

- `problem`: what is being solved or decided, constraints, and the evidence
  to inspect (absolute file paths, diffs, command output, docs). Do not include
  your conclusion here.
- `candidate`: your current solution and a numbered list of claims to verify.

## 2. Run exactly this program in `fabric_exec`

Pass `problem` and `candidate` as payloads. Set `timeoutMs` on the
`fabric_exec` call to at least 1500000. Do not change the registry, tools,
`extensions`, or transport.

```ts
const REGISTRY = { astra: "openai-codex/gpt-6-astra" } as const;
const reviewer = "astra";
const expected = REGISTRY[reviewer];
const DEADLINE_MS = 20 * 60 * 1000;

if (!(await pi.bash({ cmd: 'test "$HERDR_ENV" = 1', settle: true })).ok) {
  return { reviewer, status: "error", error: "Oracle requires Herdr observability. Start the parent Pi session inside Herdr." };
}

const task = `Act as an independent technical reviewer. Do not assume the candidate is correct. Do not edit files.

PART A — PROBLEM AND EVIDENCE:
${π.problem}

First analyze Part A independently and write down what you believe the correct behavior or solution is, BEFORE reading Part B.

PART B — CANDIDATE SOLUTION AND CLAIMS:
${π.candidate}

Then compare Part B against your independent analysis. Report:
1. Your independent conclusion
2. Confirmed claims
3. Incorrect claims
4. Unsupported assumptions
5. Edge cases
6. Evidence (file:line or doc references)
7. Recommended corrections
8. Remaining uncertainty
Prefer primary source code and official documentation.`;

const h = await agents.spawn({
  name: `oracle-${reviewer}`, model: expected, thinking: "high",
  tools: ["read", "grep", "find", "ls"], extensions: false, transport: "herdr", task,
});
if (h.model !== expected) {
  await agents.stop({ id: h.id }).catch(() => {});
  return { reviewer, status: "error", error: `model mismatch: expected ${expected}, got ${h.model}` };
}

const timer = new Promise<null>(r => setTimeout(() => r(null), DEADLINE_MS));
const r = await Promise.race([agents.wait({ id: h.id }), timer]);
if (r === null) {
  await agents.stop({ id: h.id }).catch(() => {});
  return { reviewer, model: expected, id: h.id, attach: h.attachCommand, status: "timeout", result: null };
}
if (r.model !== expected) {
  return { reviewer, status: "error", error: `result model drift: ${r.model}` };
}
if (r.status !== "completed" || !r.text) {
  return { reviewer, model: expected, id: h.id, attach: h.attachCommand, status: r.status, error: r.error ?? "empty result", result: null };
}
return { reviewer, model: expected, id: h.id, attach: h.attachCommand, status: "completed", result: r.text };
```

## 3. Report

- Present the review attributed to the reviewer and model; do not merge it
  into an anonymous answer.
- List disagreements with your candidate explicitly, then verify disputed
  claims yourself before changing your conclusion.
- A `timeout`, `failed`, `stopped`, or `error` status is not a review; report
  it with the attach command and do not retry automatically.
