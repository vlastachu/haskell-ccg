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
        1.a                    1.b                    2                      3.a                    3.b
        .                      .                      .                      .                      .
       / \                    / \                    / \                    / \                    / \
      /\  \                  /\  \                  /   \                  /  /\                  /  /\
     /  \  \                /  \  \                /     \                /  /  \                /  /  \
    /\   \  \              /   /\  \              /\     /\              /  /\   \              /  /   /\
   /  \   \  \            /   /  \  \            /  \   /  \            /  /  \   \            /  /   /  \
The roses are red      The roses are red      The roses are red      The roses are red      The roses are red
```

For square chart it would look like:

```
[.] [ ] [ ] [ ] 
[1] [3] [ ] [ ] 
[2] [ ] [2] [ ] 
[3] [ ] [ ] [1] 

Derivation of cell 4x1 = 
    (try combination rules on 1) U (try combination rules on 2) U (try combination rules on 3)

Ambigious of a and b paths for 1 and 3 cases is created by cells 3x1 and 3x2 which have 2 paths of parsing.
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
- [x] Implement `Coordination` rule with two `CONJ` categories. 
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

## Papers and links

- CCG
    - Mark Steedman "A Very Short Introduction to CCG"; 1996y; 8p; [link](http://www.inf.ed.ac.uk/teaching/courses/nlg/readings/ccgintro.pdf)
    - Mark Steedman and Jason Baldridge "COMBINATORY CATEGORIAL GRAMMAR"; 200Xy; 62p; [link](http://homepages.inf.ed.ac.uk/steedman/papers/ccg/SteedmanBaldridgeNTSyntax.pdf)
    - David J. Weir "COMBINATORY CATEGORIAL RELATIONSHIP TO LINEAR GRAMMARS: GENERATIVE POWER AND CONTEXT-FREE REWRITING SYSTEMS" 1988y; 8p; [link](http://www.anthology.aclweb.org/P/P88/P88-1034.pdf)
    - Not seems to be useful
        - JOACHIM LAMBEK "Type Grammars as Pregroups"; 2001y; 19p; [link](http://www.ling.ohio-state.edu/~dowty/CG/lambek-pregroups.pdf)
        - Michael Moortgat "Categorial Type Logics"; 1997y; 54p; [link](http://www.let.uu.nl/~Michael.Moortgat/personal/Courses/hlola98.pdf)
- Parsing
    - Shift-Reduce
        - Yue Zhang, Stephen Clark "Shift-Reduce CCG Parsing"; 2011y; 10p; [link](www.cl.cam.ac.uk/~sc609/pubs/acl11yue.pdf)
        - Stephen Merity and James R. Curran "Frontier Pruning for Shift-Reduce ccg Parsing"; 2011y; presentation 38p; [link](http://smerity.com/media/files/academic/alta2011_presentation.pdf)
        - STEPHEN MERITY (Curran supervisor) "Integrated Tagging and Pruning via Shift-Reduce CCG Parsing"; 2011; 104p; [link](http://smerity.com/media/files/academic/smerity_thesis.pdf)
    - CKY algorithm
        - Laura Kallmeyer, Magnus Roos "Parsing Cocke Younger Kasami" - CKY basics on CFG; [link](https://user.phil-fak.uni-duesseldorf.de/~kallmeyer/Parsing/cyk.pdf)
        - K. Vijay-Shanker, David J. Weir "POLYNOMIAL TIME PARSING OF COMBINATORY CATEGORIAL GRAMMARS"; 1990y; 8p; [link](http://www.aclweb.org/anthology/P90-1001)
        - K. Vijay-Shanker, David J. Weir "Parsing Some Constrained Grammar Formalisms" - for TAG and CCG; 1993y; 46p; [link](http://www.aclweb.org/anthology/J93-4002)
        - Bojan Djordjevic and James R. Curran "Efficient Combinatory Categorial Grammar Parsing" - CKY + Supertagging; 2006y; 8p; [link](http://rp-www.cs.usyd.edu.au/~james/pubs/ps/altw06efficiency.ps)
        - Bojan Djordjevic and James R. Curran, Stephen Clark "Improving the Efficiency of a Wide-Coverage CCG Parser"; 2007y; 9p; [link](http://www.aclweb.org/anthology/W07-2206)
        - Kurt Keutzer, Slav O Petrov, Youngmin Yi "Efficient parallel CKY parsing using GPUs"; 2011y; 266p; (no link: parsing-technologies-2011-proceedings.pdf) [11 pages?](http://www.aclweb.org/anthology/W11-2921)
        - Tanin Na Nakorn "Combinatory Categorial Grammar Parser in Natural Language Toolkit" - about type raising problem; 2009y; 59p; [link](http://www.inf.ed.ac.uk/publications/thesis/online/IM090710.pdf)
    - A* algorithm
        - Mike Lewis, Mark Steedman "A* CCG Parsing with a Supertag-factored Model"; 2014y; 11p; [link](http://www.aclweb.org/anthology/D14-1107)
        - Michael Auli, Adam Lopez "Efficient CCG Parsing: A* versus Adaptive Supertagging"; 2011y; 9p; [link](http://www.aclweb.org/anthology/P11-1158)
        - [LSTM CCG Parsing](http://homes.cs.washington.edu/~lsz/papers/llz-naacl16.pdf)
        >> Parsing models either use these scores directly (Auli and Lopez, 2011b), or as a form of beam search (Clark and Curran, 2007), typically in conjunction with models of the dependencies or derivation.
    - not categorized
        - Stephen Clark, James R. Curran "Wide-Coverage Efficient Statistical Parsing with CCG and Log-Linear Models"; 2006y; 57p; [link](www.cl.cam.ac.uk/~sc609/pubs/cl06parser.pdf)
        - Marco Kuhlmann, Giorgio Satta "A New Parsing Algorithm for Combinatory Categorial Grammar" - compares with 1990 CKY algorithm; 2014y; 14p; [link](http://www.aclweb.org/anthology/Q14-1032)
- Implementations
    - NLTK CKY [github source](https://github.com/nltk/nltk/blob/develop/nltk/ccg/chart.py)
        - Tanin NLTK changes [github blame](https://github.com/tanin47/nltk/blame/tanin-remove-unnecessary-type-raise/nltk/ccg/chart.py) (TODO: see other branches)
    - C&C Tools - (CKY?) with probabilistick ranging [svn](http://svn.ask.it.usyd.edu.au/trac/candc/) see also "Wide-Coverage Efficient Statistical Parsing with CCG and Log-Linear Models"
    - OpenCCG - CKY [github](https://github.com/OpenCCG/openccg)
    - EasyCCG - A* [github](https://github.com/mikelewis0/easyccg)
