# ADR-008: support builders consume known registrations

Status: accepted  
Date: 2026-07-26

## Decision

Surface and volume support-map builders consume coordinate sets or affines
that the caller declares to be already related. They do not estimate,
optimize, repair, or validate anatomical registration.

If two domain spaces do not share one known `space_id`, the caller must
supply a registration identifier. Different declared brain structures are
rejected.

## Rationale

This keeps support algebra separate from preprocessing and avoids making a
numeric nearest-neighbour result appear to establish anatomical
correspondence. It also preserves the no-external-binary runtime policy.

## Consequences

- builders are deterministic and auditable;
- registration quality remains an input uncertainty, not a hidden step;
- arbitrary nonlinear warp estimation is deferred;
- alternative known registrations can be compared through sensitivity APIs.
