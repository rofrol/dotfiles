# Architecture and Maintenance Policy

## Purpose

This file defines how humans and coding agents must develop and maintain this
system.

LLM agents can generate and modify code much faster than humans can review the
resulting architecture. An individual change may appear correct while gradually
increasing coupling, duplication, hidden assumptions, and the amount of context
required to understand the system.

As the codebase grows, an agent becomes less able to reason about it as a whole.
It may then solve local problems by adding adapters, abstractions, compatibility
layers, special cases, and patches. Each addition makes the next change harder
and accelerates architectural decay.

Complexity is therefore a limited resource. The goal is not to maximize the
amount of code produced. The goal is to keep the system small, coherent,
replaceable, and understandable by both humans and agents.

These rules exist to guide judgment, not to replace it with arbitrary metrics
or procedural paperwork.

## Human Responsibility

Humans retain control of the architectural core. A human must approve changes
to:

- system-wide architecture and module boundaries;
- core domain models and invariants;
- public interfaces and persistent data formats;
- measurable product and operational requirements;
- security and trust boundaries;
- decisions that would be expensive or difficult to reverse;
- rewrites that replace existing modules.

Agents may propose such changes, explain their trade-offs, and prepare a plan,
but must not silently make them.

## Classifying Changes

A change is non-trivial if it does any of the following:

- materially alters observable behavior, a public contract, or persistent
  data;
- adds or changes a dependency, module boundary, or trust boundary;
- changes concurrency, authorization, security, or failure behavior;
- introduces a new architectural concept or assumption;
- requires coordinated edits in places that can no longer be understood
  independently;
- cannot be verified safely through local reasoning and focused tests.

A change may still be trivial when it makes a small, local, easily reversible
adjustment to presentation or other non-contractual behavior, provided its
consequences can be established locally and verified directly. Otherwise, a
change is trivial only when behavior and contracts remain unchanged, no
boundary or architectural assumption changes, correctness can be established
locally, and the change is easy to reverse.

Classify changes by their consequences, not by line count, file count, function
length, or similar proxy metrics. When uncertainty about material consequences
remains after proportionate inspection, treat the change as non-trivial.

A consequence is material when it changes something a user, caller, operator,
or neighboring module may reasonably rely on; requires migration or
coordinated changes; expands authority or failure impact; introduces a durable
constraint; or would be costly to reverse.

## Rules for Agents

### 1. Protect the Architectural Core

- Keep the architectural core as small as possible.
- Prefer moving optional behavior to replaceable modules around the core.
- Do not expand the core merely to make a local implementation easier.
- Ask for human approval before changing the core or its fundamental
  assumptions.

### 2. Enforce Module Boundaries

- Draw boundaries around cohesive responsibilities and design decisions that
  may change independently, not around process steps, technical fashions, or
  arbitrary layers.
- Prefer deep modules: narrow interfaces that hide substantial complexity and
  expose the smallest useful contract. Do not create shallow wrappers whose
  interfaces expose nearly as much complexity as their implementations.
- A boundary should hide knowledge that other modules do not need. If callers
  must understand the implementation, duplicate its assumptions, or coordinate
  with its internal changes, the boundary is misplaced or incomplete.
- Define narrow, explicit interfaces between modules.
- Enforce boundaries mechanically with the language, type system, build system,
  dependency rules, linters, or architecture tests.
- Do not bypass a boundary through internal imports, shared mutable state,
  database access, reflection, or duplicated implementation knowledge.
- Treat a boundary that exists only in documentation as incomplete.

### 3. Protect Trust Boundaries

Treat the following as trust-boundary crossings unless the project explicitly
defines otherwise:

- user-controlled or externally supplied input;
- network, filesystem, subprocess, and device I/O;
- databases and other persistent state;
- secrets, credentials, and authentication material;
- third-party code, services, and dependencies;
- privileged operations and externally visible side effects.

At each trust boundary:

- validate and normalize untrusted data before it enters the core;
- keep authentication, authorization, and side effects explicit;
- use narrow representations that express what has already been validated;
- do not allow raw untrusted representations, secrets, or ambient authority to
  spread through the system;
- ask for human approval before creating, removing, or materially changing the
  boundary.

### 4. Specify Properties, Not Only Examples

- Distinguish programmer errors from expected operational failures. Encode
  programmer assumptions, preconditions, postconditions, and invariants as
  assertions or mechanically checked properties where appropriate; handle
  expected failures explicitly as part of the contract. Do not use assertions
  as a substitute for handling untrusted input or recoverable failure.
- Test system properties, contracts, and invariants in addition to example
  inputs and outputs.
- Prefer tests that remain valid when the implementation changes.
- Include failure behavior, boundary conditions, state transitions, and
  interactions between modules.
- Do not use a growing collection of narrow regression tests as a substitute
  for identifying and specifying the underlying rule.

### 5. Record Why Decisions Were Made

Record significant, durable decisions in
`docs/decisions/NNNN-short-title.md`. Create the directory when the first such
decision is needed. Use the next available sequence number and this format:

```md
# Title

## Context
What forces required a decision?

## Decision
What was chosen?

## Alternatives
What materially different options were rejected?

## Consequences
What becomes easier, harder, or constrained?
```

Use the lightest durable location appropriate to the decision:

- a cross-cutting or costly-to-reverse decision belongs in a decision record;
- a module contract belongs in documentation next to that module;
- a non-obvious local reason belongs in a comment next to the affected code;
- a temporary workaround must state why it exists and the condition for its
  removal.

Record reasons and consequences, not a chronology of implementation. Do not
create decision records for local, obvious, and easily reversible choices.
Update or supersede a record when its assumptions change; do not silently
contradict it.

### 6. Minimize the Number of Concepts

Before introducing new code or machinery, choose the first adequate solution
in this order:

1. Do not make the change when the requested behavior is unnecessary,
   speculative, or already provided.
2. Reuse an existing project mechanism that belongs within the same
   architectural boundary.
3. Use the standard library.
4. Use a native platform, language, database, or framework capability.
5. Use an already-adopted dependency only when doing so does not create new
   coupling, expose its types across a boundary, or expand its architectural
   role.
6. Otherwise, write the minimum direct implementation that satisfies the
   contract and required properties.

Use this order as a decision aid, not a code-golfing rule. Prefer the solution
with the lowest total cognitive and maintenance cost, not necessarily the
fewest lines. Read and trace the affected code before selecting a step.

- Before adding a special case, check whether changing the data representation,
  invariant, or ownership of the behavior can make it part of the normal path.
  Prefer removing exceptional paths over hiding them behind helpers or
  abstractions. Do not force unlike cases together when their contracts,
  failure behavior, or security requirements are materially different.
- Judge complexity by how much knowledge is required to understand and safely
  change the system, not by line counts, function length, file size, class
  count, or similar proxy metrics.
- Every new concept, abstraction, dependency, layer, indirection, execution
  path, and special case must justify its continuing cognitive and maintenance
  cost.
- Distinguish simple from merely easy or familiar. Reject local convenience
  when it entangles otherwise independent state, time, identity, I/O, or
  responsibilities and thereby increases global complexity.
- Prefer a direct implementation when an abstraction does not eliminate more
  complexity than it introduces.
- Prefer changes that reduce the number of concepts, execution paths, and
  places that must change together.
- Keep cohesive code together. Do not split functions, modules, or types merely
  to satisfy style metrics.
- Do not introduce interfaces, factories, wrappers, or layers solely in
  anticipation of hypothetical future requirements.
- Use hard numeric limits only for actual product, platform, or operational
  requirements, such as latency, memory use, bundle size, build time, storage,
  or external API limits.
- When such a limit exists, treat it as a concrete requirement and verify it
  directly. Do not generalize it into arbitrary code-shape rules.

### 7. Delete Code and Use Clean Cutovers

- Backward compatibility is not the default. Do not propose or add legacy
  migrations, dual reads or writes, version bridges, adapters, deprecation
  periods, fallback paths, or mitigations without explicit human approval for
  the exact need.
- Compatibility work requires a named current consumer, deployed engagement,
  or stored-state dependency. Git history, old branches, stale plans, previous
  local runs, tests alone, and hypothetical future users do not justify it.
- When replacing behavior, update all current producers and consumers, remove
  the old implementation, and recreate disposable development data instead of
  building migration machinery around it.
- Treat every legacy path as a removal candidate, not as automatically safe to
  delete. Before removal, identify the behavior it supports; direct callers and
  configuration references; dependent tests; migration or stored-state
  dependencies; the replacement path; and an observable check proving the old
  path is no longer needed.
- Remove a path only when its required behavior is covered by the replacement
  and the relevant checks pass. Report the files removed, evidence inspected,
  replacement verified, and anything preventing conclusive removal.
- If removal cannot be justified, preserve the current path and report the
  missing evidence. Do not add another compatibility layer to compensate for
  uncertainty.
- Do not retain speculative abstractions or code for hypothetical future use.
- Measure progress by reduced complexity and delivered behavior, not by lines
  of code added.

### 8. Rewrite from Specifications When Patching Stops Paying

- Do not patch a module indefinitely.
- Recommend rewriting a module from its specification when accumulated special
  cases, compatibility layers, or hidden dependencies make safe reasoning
  difficult.
- Preserve externally required behavior through specifications, contracts, and
  tests rather than by copying the old implementation.
- Rewrite one bounded module at a time behind a stable interface.
- Require human approval before beginning a rewrite.

### 9. Keep Changes Reviewable

- Make each change small enough for a human reviewer to understand its purpose,
  behavioral effect, and architectural consequences without reconstructing
  unrelated parts of the system.
- Divide larger work along behavior or module boundaries, not arbitrary line or
  file limits.
- Keep the system working and independently verifiable after each step.
- Do not create a temporary architecture that is harder to understand than the
  final design merely to split the work.
- Keep commits coherent and arrange them so they tell the reasoning of the
  change. Do not fragment cohesive work to satisfy a commit-size metric.
- If a change cannot be made reviewable without first resolving an
  architectural question, stop and ask.

## Required Change Procedure

This procedure is a reasoning discipline, not a reporting checklist. Perform it
to the depth warranted by the consequences and uncertainty of the change.
Report only findings and decisions material to review; do not emit ceremonial
checklist answers.

Before implementing a non-trivial change:

1. Identify the module responsible for the behavior.
2. State the relevant contract, invariant, or specification.
3. Check whether the change crosses an architectural or trust boundary.
4. Consider whether deleting or simplifying existing code—or improving the
   data representation, invariant, or responsibility boundary—solves the
   underlying problem without adding a special path.
5. Identify any new concepts, dependencies, indirection, execution paths, or
   places that would have to change together.
6. Determine whether a human decision or a durable decision record is required.
7. Plan the smallest coherent and reviewable change.
8. Verify properties and invariants, not only the requested example.
9. Remove code and temporary compatibility paths made obsolete by the change.

For a trivial change, use proportionate judgment: verify it locally and report
what changed. Do not produce ceremonial documentation or empty checklist
answers.

## Stop and Ask

Stop implementation and ask for human direction when:

- the specification is missing, ambiguous, or contradicted by existing
  behavior in a way that materially affects the implementation choice;
- the change requires materially altering the architectural core, a public or
  cross-cutting module boundary, or a trust boundary;
- two locally reasonable solutions create materially different long-term
  architectures;
- a new dependency, abstraction, adapter, compatibility layer, or special case
  adds continuing cognitive cost without clearly removing greater complexity;
- safe implementation requires understanding more of the system than can be
  reliably established after proportionate investigation;
- patches are treating symptoms while the module's design is the underlying
  problem;
- the change is costly to reverse or its consequences cannot be reviewed
  confidently.

Before stopping, perform safe, read-only investigation proportionate to the
decision: inspect the responsible code and its callers, existing contracts,
tests, decision records, and relevant history when available. Do not ask the
human to resolve a question that the repository can answer directly. Do not,
however, turn investigation into an unbounded attempt to avoid a necessary
decision.

Investigation is proportionate when the agent has inspected the responsible
code, relevant callers, contracts, tests, and decision records needed to
resolve the specific uncertainty. It need not reconstruct unrelated parts of
the system.

Do not conceal material uncertainty by generating more code. Silence is not
approval.

If human approval is required but unavailable:

- do not make the blocked change;
- record the unresolved question and the options considered;
- continue only with independent, reversible work that does not assume an
  answer;
- prepare analysis, tests of existing behavior, or an unapplied proposal when
  useful;
- do not weaken, bypass, or reinterpret the approval requirement to keep the
  task moving.

## Verification and Reporting

Make compliance auditable without creating ritual paperwork.

When treating a potentially boundary-affecting change as non-material, state
the reason briefly in the change summary. The justification must refer to the
actual contract, affected callers, reversibility, or failure impact; merely
labeling the change "internal" or "small" is insufficient.

For a trivial change, report the change and the verification performed.

For a non-trivial change, include a concise summary in the final response or
pull-request description:

```md
## Change Summary

- Behavior changed:
- Contracts or boundaries affected:
- Concepts or dependencies added or removed:
- Verification performed:
- Decisions recorded:
- Remaining uncertainty or required human decision:
```

Omit fields that genuinely do not apply rather than filling them with
boilerplate. The summary must reflect actual reasoning and verification, not
serve as a substitute for them.

## Guiding Principle

Before adding a new abstraction, dependency, adapter, compatibility layer, or
special case, consider whether deleting, simplifying, or replacing existing
code would solve the underlying problem.

Optimize for the long-term cost of understanding and changing the system, not
for the short-term speed of producing code.

## Intellectual Provenance

This policy is a synthesis, not a claim that any one source originated each
idea. Many principles were discovered independently and overlap. The names
below identify the strongest influences on each part. GPT and Claude are listed
as drafting or review contributors, not as the originators of established
software-engineering principles.

| Policy area | Principal influences | Contribution to this policy |
| --- | --- | --- |
| Complexity as a limited resource | Grug Brain, John Ousterhout, Rich Hickey | Minimize the knowledge and entanglement required to understand and change the system. |
| Human control of architectural decisions | GPT synthesis, refined through Roman Frołow's review | Reserve costly, cross-cutting, security-sensitive, and hard-to-reverse decisions for humans. |
| Trivial and non-trivial changes | Claude critique, GPT drafting | Classify by consequences rather than line or file counts; define material consequences and proportionate investigation. |
| Architectural core and replaceable surroundings | David Parnas, John Ousterhout, GPT synthesis | Keep the core small and isolate optional behavior behind stable boundaries. |
| Module boundaries and deep modules | David Parnas, John Ousterhout | Divide by cohesive responsibility and hidden design decisions; prefer narrow interfaces that conceal substantial complexity. |
| Trust boundaries | Security-engineering practice, GPT synthesis | Validate and narrow external data, authority, credentials, dependencies, and side effects before they enter the core. |
| Contracts, invariants, and assertions | Brad Fitzpatrick, John Carmack, Tiger Style | Test properties rather than only examples; distinguish programmer errors from expected operational failures. |
| Decision records and explanations | Michael Nygard's ADRs, Tiger Style | Record durable reasons, alternatives, and consequences without creating ceremonial documentation. |
| Minimum-concept rule and late abstraction | Grug Brain, YAGNI, Casey Muratori, Jonathan Blow | Prefer direct code; introduce an abstraction only when it removes more complexity than it creates. |
| Minimal-solution ladder | Ponytail, adapted by GPT and Roman Frołow's review | Try no change, existing project code, standard library, native capability, safe reuse of a dependency, then minimum direct implementation. |
| Simple versus merely easy | Rich Hickey | Reject local convenience that entangles independent state, time, identity, I/O, or responsibilities. |
| Removing special cases through representation | Linus Torvalds | Improve data representation, invariants, or ownership so exceptional paths become normal paths where semantics permit. |
| Data, hardware costs, and measurable limits | Casey Muratori, Bill Hall, Tiger Style | Treat real latency, memory, storage, build, and platform limits as requirements while rejecting arbitrary code-shape metrics. |
| Dependencies and hidden machinery | Bill Hall, Grug Brain, Ponytail, Jonathan Blow | Account for continuing coupling and maintenance cost; do not add machinery merely for short-term convenience. |
| Deletion and bounded rewrites | Grug Brain, Ponytail, John Ousterhout, GPT synthesis | Delete obsolete paths and rewrite a bounded module from its contract when continued patching no longer pays. |
| Compatibility and clean cutovers | [Philipp Schmid](https://x.com/_philschmid/status/2094152154382528996), [Wagsify](https://x.com/wagsify/status/2094215533172494433), [Second Mind Systems](https://x.com/Secondmindsys/status/2094167277944082761), refined through Roman Frołow's review | Make compatibility exceptional, require a real current dependency, prove obsolete paths are replaceable before deletion, and avoid turning uncertainty into new legacy machinery. |
| Small, reviewable, working steps | Brad Fitzpatrick, Linus Torvalds | Preserve a working, independently verifiable system and organize changes around coherent behavior. |
| Stop and Ask, including unavailable humans | GPT drafting, strengthened by Claude critique | Investigate first, expose material uncertainty, block only the affected decision, and continue independent reversible work. |
| Concise verification reporting | GPT synthesis, refined through Roman Frołow's review | Make consequences and evidence auditable without substituting a checklist for reasoning. |

Roman Frołow selected, combined, and repeatedly refined these influences. GPT
produced the current integrated wording. Claude's documented role was critical
review, especially around undefined change classes, unavailable human approval,
and possible abuse of the terms "material" and "proportionate".
