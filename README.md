# haskell-ccg

This repository is a part of my science project. Haskell was chosen as language for fast prototyping 
(but some parts like iterative CYK-algorithm looks ugly). The final target of project is perfomance. Target of this concrete 
part is model of parsing algorithms for understanding of concept. If you come here from outer internet - you won't 
see there something new.

## Implementation details

Code is not well documented and probably won't. In general we should:

1. Take string from input, divide on tokens and assign list of possible categories for each of them.
2. Use parsing algorithm and derive `S` category by every possible path.
3. Save parsing path in 2. Show him for user.

The main moment in CYK algorithm is that we can implement immutable chart
in which we derive `S` Category from down (words with categories lists) to top. On each cell from second to last we select 
variants of deriving nonterminal (deriving category by any rule in ccg case). Each cell contains list of categories, 
some string info for output, links for other cells.

In most languages we have to allocate n*n array and use 3 for loops for deriving. 
First loop add line from down to top in chart (easy to impliment in immutable language).
Second loop adds cells from right to left (also easy). Let's consider what's hapenning in 3'd loop (filling of cell).
For example take 4'th row 1'st column. We should try use rules on categories from 3 possible ways of derivation. 

```
        .                       .                          .
       / \                     / \                        / \
      /\  \                   /   \                      /  /\
     /  \  \                 /     \                    /  /  \
    /\   \  \               /\     /\                  /  /   /\
   /  \   \  \             /  \   /  \                /  /   /  \
The roses are red       The roses are red          The roses are red
```

For square chart it would look like:

```
[.] [ ] [ ] [ ] 
[3] [1] [ ] [ ] 
[2] [ ] [2] [ ] 
[1] [ ] [ ] [3] 

Derivation of cell 4x1 = (try combinators on 1) U (try combinators on 2) U (try combinators on 3)
```

In simple words derivation for cell is like zipping between column list and reversed diagonal list. So I implement 
structure which holds links on down, right and down-right diagonal and may give column-, or row-, or diagonal-list.
Then we may use applicative ap or list comprehension to enumerate all possible combinations.

**Parsing paths is probably overbloated**. Each combinator in cell keeps full path of derivation.

## Roadmap

- [x] Combinators representation
- [x] CYK algorithm
- [x] Parse path
- [x] Simple parsing path output
- [ ] Implement `Coordination` rule with two `CONJ` categories. 
- [ ] Take any popular ccg vocabulary format as part of input
- [ ] Write some test scenaries which take more time.
- [ ] Remove fake paths
- [ ] Division on json output and js render 
- [ ] Full browser interface (input phrase and grammar and fancy things in output)
- [ ] A* algorithm (additional input with probabilities needed)
- [ ] Maybe analysis of efficiency of parallel execution on haskell

## Install and use

Edit `sample` in code (there is no input interface now) in format [("word", [combinators...])...].

```
stack setup
stack install
haskell-ccg-exe
```

Open `output.html`. You should see something like this:

![output.html](http://i.imgur.com/6qbnmCk.png?1)
