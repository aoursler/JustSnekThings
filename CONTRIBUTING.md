# GH Protocol

## Protocol for building a New Thing

0. When you start a New Thing, first you need to pull master. Make sure you're
on your local master with `git checkout master`, and then run `git pull` to get
the most recent version of master you can.

1. Make a branch with `git checkout -b my-branchname`, and call it something
descriptive, like `add-gh-protocol`. As we move forward, we can also choose
to call the branch names `<yourname>-<descriptive-name>`, but for now I think
it's fine the way it is.

2. Do your work, committing to the branch every time you are at even the very
smallest of checkpoints. If you write one function and it works, commit! The
commands for this are `git add .` (where the dot adds the whole folder,
but you can specify file name(s) too if you wish to only add a file or two).
Then, `git commit -m "your commit message here"`. Make it descriptive, and
use past tense, like "added foo function", or "fixed x thing".

3. When you're done for some amount of time, (usually for me when I leave
my laptop or go to class or something), push to your branch! The first time
you do this with a new branch, you'll have to run
`git push --set-upstream origin <your branch name>`, but after that it should
be just `git push`.

4. When you have any amount of code pushed, even just the first thoughts, go
to GH and open a pull request, requesting to merge your new branch into master.
You won't actually merge your branch until you're finished your thing, don't
worry, but this PR will be a place for you to keep notes to yourself if you'd
like as well as communicate with us as your teammates about where you're at
and what you're thinking.

5. Repeat steps 2 and 3 as you work.

6. When you're ready to merge your branch, make a comment on your PR saying so,
and tag both of us so that we get notifications for that.

7. We'll communicate a bit, and then you'll merge! You are now finished with
the New Thing, and you can delete your branch! Congrats! Don't forget to move
the card in the project tab from `in progress` to `done`.

## Other GitHub stuff:

- Issues are great for conversations and logging bugs. We'll come to these
when we have things to do, honestly.

- Use the project to keep track of todos and things you're working on!
