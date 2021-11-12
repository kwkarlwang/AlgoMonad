# AlgoMonad

## Author

Karl Wang

## Idea

AlgoMoond is a haskell TUI that allows user to download LeetCode questions through the TUI to their local machines, edit them in their favorite editors, ansubmit them through the TUI.

The motivation to create this program is that LeetCode only allows the user to edit and submit the code on their website. However, programmers tend to want to edit code in their favorite editors instead of in the browser. Hence, I would like to create a interface that allows the user to download to local machine and submit from local machine.

## Libraries

Here is a list of libraries needed for this program

- [req](https://hackage.haskell.org/package/req): use to make request
- [brick](https://hackage.haskell.org/package/brick): use to make TUI

## Goals

- [ ] Acquire user token automatically through browser cookies.
- [ ] Browse LeetCode questions by categories.
- [ ] Download and write LeetCode questions to local files.
- [ ] Submit local files to LeetCode and evaluate result.

## Inspirations

- [leetcode-cli](https://github.com/skygragon/leetcode-cli): Main source of inspiration. However, this is just a pure cli instead of a TUI. Implemented in javascript
- [vscode-leetcode](https://github.com/LeetCode-OpenSource/vscode-leetcode): A GUI for LeetCode. However, only available on VSCode. Implemented in javascript.
