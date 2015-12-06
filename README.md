# CL-PERMUTATION

A library for operating on permutations and permutation groups.


## Creating Permutations

Permutations are represented by the structure `PERM`, which is
read-only/immutable at the API boundary. A permutation of size `N` is essentially a sequence
of numbers from `1` to `N`. One-based permutations were chosen because that
is the dominating convention in mathematics. All we lose, essentially,
is direct compatibility with array indexing, and one fixnum worth of
space. (Internally, the permutations are stored in an array of size
`N+1`, where the zeroth element is always zero).

A permutation can be created via `MAKE-PERM`:

```
PERM> (make-perm 1 2 3)
#<PERM 1 2 3>
```

The permutation will be checked for validity.

```
PERM> (make-perm 1 2 5)
Given permutation must contain the numbers 1 to 3
   [Condition of type SIMPLE-ERROR]
```

One can also create permutations with `LIST-TO-PERM`, which converts a
list to a permutation. The companion function `PERM-TO-LIST` does the
opposite operation, but it's not recommended to use list
representations of permutations.

One can also create permutations with `VECTOR-TO-PERM`, which is
analogous to `LIST-TO-PERM`, except it works for vectors. The reverse is
`PERM-TO-VECTOR`.

Lastly, there is an experimental reader macro for permutations, which
are created at read time. To enable the syntax, use

```
(enable-perm-reader)
```

and then one may type

```
#[3 1 2 4 5]
```

for permutations instead.


## Permutation Operations

There is a slew of permutation operations:

* `perm-identity`: construct an identity perm
* `perm-identity-p`: check if a perm is an identity perm
* `random-perm`: construct a random perm with specified parity
* `perm-ref`: zero-based reference
* `perm-eval`: one-based (standard) reference
* `perm-eval*`: one-based (standard) reference with out-of-bounds handling
* `perm-inverse-eval`: one-based (standard) reference of inverse
* `perm-inverse-eval*`: one-based (standard) reference of inverse with out-of-bounds handling
* `perm=`: check for equality
* `perm=*`: check for equality of different sized perms
* `perm-size`: the size of the permutation (number of mapped elements)
* `perm-length`: number of inversions
* `perm-even-p`: check for evenness/oddness
* `perm-odd-p`: ''
* `perm-sign`: ''
* `perm-compose`: compose two permutations
* `perm-expt`: compose a perm with itself a number of times
* `perm-order`: order of a permutation
* `perm-transpose-indexes`: swap two indexes, keeping the entries fixed
* `perm-transpose-entries`: swap two entries, keeping the indexes fixed
* `perm-inverse`: invert a permutation
* `perm-fixpoints`: compute the fixed points of a permutation
* `permute`: permute an array according to a permutation


## Permutation Generation

There are ways of efficiently generating all permutations of a given
length incrementally. Instead of generating all permutations at once
in memory -- which takes `O(n*n!)` space -- we generate permutations on
the fly.

The first way is to iterate over the permutations using a `DOLIST`-style
macro called `DOPERMS`.

```
PERM> (let ((i 1))
        (doperms (p 3)
          (format t "~D: ~A~%" i p)
          (incf i)))
1: #<PERM 1 2 3>
2: #<PERM 1 3 2>
3: #<PERM 3 1 2>
4: #<PERM 3 2 1>
5: #<PERM 2 3 1>
6: #<PERM 2 1 3>
```

The other way is to produce a generator object (a closure, in fact)
which generates the permutations. Simply `FUNCALL` the object to receive
the next permutation. When they're all exhausted, the closure will
return `NIL`.

```
PERM> (defparameter S3 (make-perm-generator 3))
S3
PERM> (defparameter S2 (make-perm-generator 2))
S2
PERM> (list (funcall S2) (funcall S3))
(#<PERM 1 2> #<PERM 1 2 3>)
PERM> (list (funcall S2) (funcall S3))
(#<PERM 2 1> #<PERM 1 3 2>)
PERM> (list (funcall S2) (funcall S3))
(NIL #<PERM 3 1 2>)
PERM> (list (funcall S2) (funcall S3))
(NIL #<PERM 3 2 1>)
PERM> (list (funcall S2) (funcall S3))
(NIL #<PERM 2 3 1>)
```


## Cycle Operations

There's also a number of operations for cycles. Cycles are represented
by the `CYCLE` structure. We can convert to and from cycle
representation using `TO-CYCLES` and `FROM-CYCLES`. Cycles created by
`TO-CYCLES` are automatically canonicalized with
`CANONICALIZE-CYCLES`. Canonicalization is defined as:

  * Cycles contain their least element positionally first.
  * Cycles are listed in descending order of their first element.
  * No null cycles exist.
  * The sum of the cycle lengths of a decomposition of a permutation
    of size `N` is `N`.

Cycles that have not been canonicalized are printed with an
asterisk '`*`'. We can observe this by explicitly disabling cycle
canonicalization:

```
PERM> (make-cycle 3 1)
#<CYCLE (1 3)>                ; no asterisk
PERM> (let ((*canonicalize-cycle-on-creation* nil))
        (make-cycle 3 1))
#<CYCLE (3 1)*>               ; asterisk
```

An example use of `TO-CYCLES` is as follows:

```
PERM> (let ((r (random-perm 10)))
        (values r (to-cycles r)))
#<PERM 7 4 8 5 2 10 3 9 1 6>
(#<CYCLE (6 10)> #<CYCLE (2 4 5)> #<CYCLE (1 7 3 8 9)>)
```

`FROM-CYCLES` allows the specification of the permutation's length. For example:

```
PERM> (from-cycles (list (make-cycle 1 3 2)))
#<PERM 3 1 2>
PERM> (from-cycles (list (make-cycle 1 3 2)) 5)
#<PERM 3 1 2 4 5>
```

Lastly, there is a (mostly useless) function `CYCLES-TO-ONE-LINE` which
converts cycles to one-line notation. That is, the cycles

```
(1 2 3)(4 5)
```

gets converted to the permutation

```
12345.
```

For example,

```
PERM> (cycles-to-one-line (list (make-cycle 1 2 3)
                                (make-cycle 4 5)))
#<PERM 1 2 3 4 5>
```

If one takes a permutation which has been canonically decomposed into
cycles, then interestingly, there exists a bijection between one-line
notation and the cycle decomposition.


## Combinatorial Specifications

A "combinatorial specification" describes a space of combinatorial
objects. They have a nice property that they all can be mapped to and
from integers sharply. See the section "Ranking and Unranking
Combinatorial Specifications".


Currently, only objects of linear structure exist. All of them are
represented as subclasses of `COMBINATORIAL-SPEC`. They are as follows.


### `RADIX-SPEC`: Base-`B` Non-Negative Integers

These are a representation of a base-`B` non-negative integer, for a
base `B > 1`. They are handled by the `RADIX-SPEC` class. Within
`CL-PERMUTATION`, the digits are written left-to-right to correspond
with natural vector index ordering. A `RADIX-SPEC` can be made with
`MAKE-RADIX-SPEC`. Here is the enumeration of all two-digit trinary
numbers:

```
PERM> (print-objects-of-spec (make-radix-spec 3 2))
0 ==> #(0 0) ==> 0
1 ==> #(1 0) ==> 1
2 ==> #(2 0) ==> 2
3 ==> #(0 1) ==> 3
4 ==> #(1 1) ==> 4
5 ==> #(2 1) ==> 5
6 ==> #(0 2) ==> 6
7 ==> #(1 2) ==> 7
8 ==> #(2 2) ==> 8
```

### `MIXED-RADIX-SPEC`: Non-Negative Mixed-Radix Integers

A mixed-radix integer is a generalization of a base-`B` integer. The
digits in a mixed-radix numeral correspond to different
bases. Mixed-radix specifications can be made with
`VECTOR-TO-MIXED-RADIX-SPEC`. For example, the following are all
numerals of radix `(2, 3, 1)`:

```
PERM> (print-objects-of-spec (vector-to-mixed-radix-spec #(2 3 1)))
0 ==> #(0 0 0) ==> 0
1 ==> #(1 0 0) ==> 1
2 ==> #(0 1 0) ==> 2
3 ==> #(1 1 0) ==> 3
4 ==> #(0 2 0) ==> 4
5 ==> #(1 2 0) ==> 5
```
Notice again we use vector index ordering.


### `PERM-SPEC`: Permutations

The space of permutations of length `N` (also known as `S_N`) can be
represented. These are represented by the `PERM-SPEC` class.

```
PERM> (print-objects-of-spec (make-perm-spec 3))
0 ==> #(0 1 2) ==> 0
1 ==> #(0 2 1) ==> 1
2 ==> #(1 0 2) ==> 2
3 ==> #(1 2 0) ==> 3
4 ==> #(2 0 1) ==> 4
5 ==> #(2 1 0) ==> 5
```

Currently, actual `PERM` objects are *not* generated (see below about
ranking/unranking). However, one can easily convert between the two.


### `COMBINATION-SPEC`: Combinations

Combinations represent the selection of `M` objects from a collection of
`N` objects. These are represented by a vector containing `M` `1`'s and `N`
`0`'s. The class that manages this is a `COMBINATION-SPEC`. For example,
all combinations of 2 objects of a total of 4 can be listed by the
following:

```
PERM> (print-objects-of-spec (make-combination-spec 4 2))
0 ==> #(0 0 1 1) ==> 0
1 ==> #(0 1 0 1) ==> 1
2 ==> #(1 0 0 1) ==> 2
3 ==> #(0 1 1 0) ==> 3
4 ==> #(1 0 1 0) ==> 4
5 ==> #(1 1 0 0) ==> 5
```

### `WORD-SPEC`: Words

A word is similar to a permutation except that it may have repeated,
indistinct elements. These are represented by a `WORD-SPEC`. It can be
created by supplying a sample word to the function
`VECTOR-TO-WORD-SPEC`. For example, all words of the form `1123` can be
listed as follows:

```
PERM> (print-objects-of-spec (vector-to-word-spec #(1 1 2 3)))
0 ==> #(1 1 2 3) ==> 0
1 ==> #(1 1 3 2) ==> 1
2 ==> #(1 2 1 3) ==> 2
3 ==> #(1 2 3 1) ==> 3
4 ==> #(1 3 1 2) ==> 4
5 ==> #(1 3 2 1) ==> 5
6 ==> #(2 1 1 3) ==> 6
7 ==> #(2 1 3 1) ==> 7
8 ==> #(2 3 1 1) ==> 8
9 ==> #(3 1 1 2) ==> 9
10 ==> #(3 1 2 1) ==> 10
11 ==> #(3 2 1 1) ==> 11
```


## Ranking and Unranking Combinatorial Specifications

Each combinatorial specification represents a finite space of `N > 0`
objects. `N` is called the "cardinality" of the specification and can be
computed with the `CARDINALITY` method.

```
> (cardinality (make-perm-spec 3))
6
> (cardinality (vector-to-word-spec #(1 1 2 3)))
12
```

The cardinality is computed only once for a combinatorial
specification and is then cached for fast access.

Obviously, every object in a particular finite combinatorial space can
be bijected to and from integers below the cardinality of that
space. `CL-PERMUTATION` provides fast and efficient mechanisms for
computing one such bijection for each combinatorial
specification. Mapping from an object to an integer is called
"ranking" and mapping from an integer back to an object is called
"unranking".

When a lexicographic ordering makes sense, there will be 1-to-1
correspondence with the ordering on integers. In other words for
objects `X1` and `X2` and their ranks `R1` and `R2`, `X1 lex< X2` iff `R1 < R2`.

The method `UNRANK` takes a combinatorial specification and an integer,
and maps it to the corresponding object representation (usually a
vector). It takes an optional keyword argument `:SET` which acts as a
destination of the unranked object (for efficiency purposes).

The method `RANK` takes a combinatorial specification and an object
produced by `UNRANK` (again, usually a sensible vector) and returns the
integer (the "rank") of that object. `PRINT-OBJECTS-OF-SPEC`, as used
above, prints the rank of every object in a combinatorial space.

One can map over all objects and ranks by using `MAP-SPEC`, which takes
a binary function (rank and object) as well as a combinatorial
specification, and applies that function to each object and their
rank.


## Permutation Groups

There is initial support for permutation groups at the
moment. Permutation groups are represented by the structure
`PERM-GROUP`.

We can create a permutation group from its generators via
`GENERATE-PERM-GROUP`. A shorthand syntax is provided which, instead of
taking a list of `PERM` objects, takes a list of lists representing
perms. This shorthand is `GROUP-FROM`. For example, the following two
are the same group:

```
PERM> (generate-perm-group (list (make-perm 1 3 2 4)
                                 (make-perm 3 2 4 1)))
#<PERM-GROUP of 2 generators>
PERM> (group-from '((1 3 2 4)
                    (3 2 4 1)))
#<PERM-GROUP of 2 generators>
```

We can generate a permutation group from a list of cycles as well. The
above is equivalent to

```
PERM> (group-from-cycles (list (list (make-cycle 2 3))
                               (list (make-cycle 1 3 4)))
                         4) 
#<PERM-GROUP of 2 generators>
```

Once we have generated a group, we can do some operations on it.

For example, let's define the group for 3x3 Rubik's cubes. A cube has
six moves: we can turn the front, back, left, right, top, and
bottom. Label each sticker with a number like so:

```
                     +--------------+
                     |              |
                     |  1    2    3 |
                     |              |
                     |  4   up    5 |
                     |              |
                     |  6    7    8 |
                     |              |
      +--------------+--------------+--------------+--------------+
      |              |              |              |              |
      |  9   10   11 | 17   18   19 | 25   26   27 | 33   34   35 |
      |              |              |              |              |
      | 12  left  13 | 20 front  21 | 28 right  29 | 36  back  37 |
      |              |              |              |              |
      | 14   15   16 | 22   23   24 | 30   31   32 | 38   39   40 |
      |              |              |              |              |
      +--------------+--------------+--------------+--------------+
                     |              |
                     | 41   42   43 |
                     |              |
                     | 44  down  45 |
                     |              |
                     | 46   47   48 |
                     |              |
                     +--------------+
```

Each turn corresponds to a permutation of stickers. I'll do the hard
work of specifying them:

```
(defparameter rubik-3x3
  (group-from
   '((3 5 8 2 7 1 4 6 33 34 35 12 13 14 15 16 9 10 11 20 21 22 23 24 17 
      18 19 28 29 30 31 32 25 26 27 36 37 38 39 40 41 42 43 44 45 46 47 48)
     (17 2 3 20 5 22 7 8 11 13 16 10 15 9 12 14 41 18 19 44 21 46 23 24 
      25 26 27 28 29 30 31 32 33 34 6 36 4 38 39 1 40 42 43 37 45 35 47 48) 
     (1 2 3 4 5 25 28 30 9 10 8 12 7 14 15 6 19 21 24 18 23 17 20 22 43 
      26 27 42 29 41 31 32 33 34 35 36 37 38 39 40 11 13 16 44 45 46 47 48) 
     (1 2 38 4 36 6 7 33 9 10 11 12 13 14 15 16 17 18 3 20 5 22 23 8 27 
      29 32 26 31 25 28 30 48 34 35 45 37 43 39 40 41 42 19 44 21 46 47 24) 
     (14 12 9 4 5 6 7 8 46 10 11 47 13 48 15 16 17 18 19 20 21 22 23 24
      25 26 1 28 2 30 31 3 35 37 40 34 39 33 36 38 41 42 43 44 45 32 29 27) 
     (1 2 3 4 5 6 7 8 9 10 11 12 13 22 23 24 17 18 19 20 21 30 31 32 25
      26 27 28 29 38 39 40 33 34 35 36 37 14 15 16 43 45 48 42 47 41 44 46))))
```

Now we have our group:

```
PERM> rubik-3x3
#<PERM-GROUP of 6 generators>
```

Let's query the group's order:

```
PERM> (group-order rubik-3x3)
43252003274489856000
```

A lot of positions! Let's generate a random cube:

```
PERM> (random-group-element rubik-3x3)
#<PERM 1 20 24 39 12 40 29 41 9 47 46 21 45 11 34 8 14 36 22 31 44 25 10 48
       16 37 43 15 26 32 7 33 30 13 35 5 28 27 23 17 19 4 38 2 18 6 42 3>
```

And as cycles...

```
PERM> (to-cycles *)
(#<CYCLE (35)>
 #<CYCLE (30 32 33)>
 #<CYCLE (27 43 38)>
 #<CYCLE (9)>
 #<CYCLE (8 41 19 22 25 16)>
 #<CYCLE (6 40 17 14 11 46)>
 #<CYCLE (4 39 23 10 47 42)>
 #<CYCLE (3 24 48)>
 #<CYCLE (2 20 31 7 29 26 37 28 15 34 13 45 18 36 5 12 21 44)>
 #<CYCLE (1)>)
```

Let's check if flipping an edge piece is valid:

```
PERM> (group-element-p (from-cycles (list (make-cycle 7 18)) 48) rubik-3x3)
NIL
```

No it's not. How about four edge pieces?

```
PERM> (group-element-p (from-cycles (list (make-cycle 7 18)
                                          (make-cycle 2 34)
                                          (make-cycle 4 10)
                                          (make-cycle 5 26))
                                    48)
                       rubik-3x3)
T
```

As can be seen, the few operations we have are powerful in studying
finite groups.

