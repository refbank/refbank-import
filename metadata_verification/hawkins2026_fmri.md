# hawkins2026_fmri

**Source:** No manuscript, preprint, or write-up exists (per dataset owner,
2026-08-27). However, the private GitHub repo `hawkrobe/tangrams-fmri`
(`task` subfolder) holds the actual Meteor/Empirica task application code
that ran the study — accessed 2026-08-27 via `gh` after the user
authenticated their own GitHub account. This is source code, not a paper,
but it directly implements the procedure, so several fields below are now
verifiable against it even without a citable manuscript.

## Citation check

- `full_cite` = "Hawkins, R. D., et al (2026) Unpublished hyperscanning
  study." and `short_cite` = "Hawkins et al. (2026)" are placeholders since
  nothing is published yet. There is no document to check author spelling,
  year, title, or venue against, so no citation check can be performed —
  this is noted rather than marked VERIFIED or FLAGGED.

## Condition-level fields

Only one condition row is recorded (condition_label = "fmri"). Fields below
are VERIFIED where the task source code directly implements them, and
UNKNOWN where the code doesn't speak to the value one way or the other
(mostly participant-demographic facts a task app has no reason to encode).

- **group_size = 2** — Confirmed by dataset owner (counted directly from the
  task code): the code never literally asserts "exactly 2," but
  `callbacks.js` always looks up exactly `players[0]` for the
  response/correctness, `Task.jsx` renders a single fixed "speaker"/
  "listener" role pair, and the study is externally described (dataset name,
  tangrams-fmri-data README) as a two-person "hyperscanning" design.
- **language = English** — Confirmed by dataset owner (2026-08-27), based on
  the (English) transcripts — not stated in the task code itself.
- **prior_relationship = mixed** — Confirmed by dataset owner (2026-08-27);
  updated from "yes" to "mixed", same rationale as `bangerter2000_reuse`'s
  fix.
- **partner_constancy = yes** — Confirmed by dataset owner (2026-08-27).
  Consistent with the code: `onGameStart` assigns one `game.get('roleList')`
  per player for the whole game, with no code path that reassigns a player
  to a different partner mid-game.
- **population = adult** — Confirmed by dataset owner (2026-08-27).
- **role_constancy = no** — VERIFIED. `callbacks.js`'s `onRoundStart`:
  `player.set('role', player.get('roleList')[round.index])` — each
  player's role is looked up fresh per round from a list indexed by round
  number, i.e. the describer/listener assignment can (and, per the
  `Task.jsx` "You are the {role}" UI showing both role labels, does) change
  across rounds within a session.
- **confederates = no** — Confirmed by dataset owner (2026-08-27). Consistent
  with the code: it treats all connected players identically — there's no
  special-cased experimenter/confederate player type.
- **modality = oral-remote** — VERIFIED (changed 2026-08-27 from
  "oral-in-person"; the original import.R already carried a
  `# idk are scanners in person?` comment flagging this as an open
  question). Multiple independent lines of evidence: (1) the public
  data-release repo `tangrams-fmri-data`'s `CHANGES.md` logs "prisma-side"
  vs. "skyra-side" run labels — Prisma and Skyra are two distinct Siemens
  MRI scanner models, so dyad members were scanned simultaneously in two
  separate scanners; (2) the task client code has no chat/messaging
  component anywhere (`client/game` only has `Round.jsx`, `Tangram.jsx`,
  `Task.jsx`, `Breadcrumb.jsx`, `WaitingForServer.jsx` — no `Chat.jsx`),
  confirming communication is spoken, not written; (3) `player.get('roomId')`
  and `player.set("scannerName", player._id)` in the code treat each player
  as belonging to a separate room/scanner identity, not a shared room; (4)
  the listener's response is a scanner-safe button-box keypress (`Digit1`/
  `Digit2`/`Digit3` in `Task.jsx`'s `handleKey`), consistent with each
  participant being alone in their own scanner bore, not sharing a physical
  input device.
- **feedback = full** — VERIFIED. `Task.jsx` has a dedicated `"feedback"`
  stage that renders `"Correct!"` or `"Ooops, this was the real target!"`
  to the player depending on whether their selection matched the target —
  explicit, immediate correctness feedback shown after every round.
- **backchannel = full** — Confirmed by dataset owner (2026-08-27), matching
  the fully-open audio intercom link between the two scanners. The task code
  confirms the channel is spoken audio (see modality above) but doesn't
  itself describe whether the link is open/two-way throughout a trial —
  that's a property of the physical intercom/scanner setup, not something
  the web app code controls or logs.

## Summary of flags

2 VERIFIED directly from the task source code (role_constancy, feedback),
1 VERIFIED via the code + owner confirmation together (modality), 7 fields
confirmed directly by the dataset owner without independent source support
(group_size, language, prior_relationship, partner_constancy, population,
confederates, backchannel), 0 FLAGGED. This dataset still has no citable
manuscript, but its actual task implementation was available (via the
private `hawkrobe/tangrams-fmri` repo) and gave direct answers for several
fields the code actually implements; the rest are participant-demographic
facts the code has no reason to encode, and rest on the dataset owner's
direct knowledge of the study instead.
