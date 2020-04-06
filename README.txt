Basic implementation of the agricola game using Haskell.

Design notes:
- Need data types to represent:
  - player boards
  - cards
  - common play area
  - overall game state:
    - current round
    - whose turn it is
    - current phase within a round

Overall strategy:
- Initialize data for a new game
- Prompt for next action
- Compute result, return next game state
- If game over, report results, else repeat

High level modules:
- function to compute possible actions
- func to compute score
- func to compute result of actions (so many funcs here)
- func to compute result of phase change