## Lando specs from `voting-system`

Some of these files highlight potential issues with the current canonical grammar or produce odd behavior, so here is some commentary on those cases.

Note that I added the keyword `relation` where necessary, since this is a relatively recent addition to the grammar. Furthermore, I removed all `plan.lando` files, as they appear to be using a syntactic structure that no longer exists.

### `should_fail`

Things that error (rightfully, according to the canonical grammar) but maybe shouldn't:
1. `bmd/bmd.lando` errors because a ':' cannot appear on the right hand side of an indexing entry.
2. `bmd/events.lando` errors because events cannot have explanations - note that if the user did not use parentheses in the attempted explanation this would silently produce unintended output.
4. `sbb/requirements.lando` errors because requirement entries can only consist of a single sentence.
5. `sbb/sbb_security.lando` and `elections.lando` error because comments are not permitted between explanations and indexing.

### `should_parse`

Things that parse silently, but maybe should warn:
1. `crypto/crypto.lando` only parses 5 components instead of 16 because empty lines are not placed between all components. Perhaps we should add a warning if the number of instances of `/\ncomponent\s/` in the input does not equal the number of parsed components, etc.?
2. `sbb/sbb_hardware.lando`, `bmd.lando`, `defcon_comms.lando`, `elections.lando`, and `polling_place_controller.lando` have the same problem as the above.
3. `logging/log.lando` does not parse a single component feature because empty lines are not placed between explanations and features. Unclear if there is an easy way to warn about this.

### Other notes

1. `bmd/requirements.lando` just has a typo.
2. `evidence_server/evidence_server.lando` just has a lot of simple mistakes - there is a fixed version in `should_parse`.
3. `logging/scenarios.lando` throws a bunch of parse errors because periods are not expected at the end of a name-phrase.
4. `sbb/sbb.lando`, although it parses, is not a good example file - it has only one component correctly formatted with an explanation, all other component features are incorrectly parsed as explanations.
5. `besspin_voting_system.lando` and `elections_reporting.lando` error because neither contains a single explanation.
