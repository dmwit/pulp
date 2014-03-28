# What is pulp?

Do you find LaTeX's output too voluminous? Do you wish it would get straight to the point and just tell you the errors and where they occurred? When you see this:

    Underfull \hbox (badness 3679) in paragraph at lines 278--282
    []\OT1/cmr/m/n/12 Shin-Cheng Mu, Zhen-jiang Hu, and
     []

    [278]
    Underfull \hbox (badness 10000) in paragraph at lines 318--323
    []\OT1/cmr/m/n/12 Yingfei Xiong,
     []

    ) [279] (./paper.aux)

    LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.

     ) )
    (\end occurred when \iftrue on line 2 was incomplete) 
    Here is how much of TeX's memory you used:
     14398 strings out of 495035
     257795 string characters out of 6181701
     330613 words of memory out of 5000000
     17235 multiletter control sequences out of 15000+600000
     23715 words of font info for 95 fonts, out of 8000000 for 9000
     14 hyphenation exceptions out of 8191
     66i,21n,72p,680b,1562s stack positions out of 5000i,500n,10000p,200000b,80000s
    </usr/share/texlive/texmf-
    dist/fonts/type1/public/amsfonts/cm/cmbx12.pfb></usr/share/texlive/texmf-dist/f
    onts/type1/public/amsfonts/cm/cmcsc10.pfb></usr/share/texlive/texmf-dist/fonts/
    type1/public/amsfonts/cm/cmex10.pfb></usr/share/texlive/texmf-dist/fonts/type1/
    public/amsfonts/cm/cmmi10.pfb></usr/share/texlive/texmf-dist/fonts/type1/public
    /amsfonts/cm/cmmi12.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfo
    nts/cm/cmmi6.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/
    cmmi7.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmmi8.p
    fb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmr10.pfb></us
    r/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmr12.pfb></usr/share
    /texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmr6.pfb></usr/share/texlive
    /texmf-dist/fonts/type1/public/amsfonts/cm/cmr7.pfb></usr/share/texlive/texmf-d
    ist/fonts/type1/public/amsfonts/cm/cmr8.pfb></usr/share/texlive/texmf-dist/font
    s/type1/public/amsfonts/cm/cmss10.pfb></usr/share/texlive/texmf-dist/fonts/type
    1/public/amsfonts/cm/cmss12.pfb></usr/share/texlive/texmf-dist/fonts/type1/publ
    ic/amsfonts/cm/cmss8.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsf
    onts/cm/cmsy10.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/c
    m/cmsy6.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmsy7
    .pfb></usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmsy8.pfb></
    usr/share/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmti10.pfb></usr/sh
    are/texlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmti12.pfb></usr/share/te
    xlive/texmf-dist/fonts/type1/public/amsfonts/cm/cmti8.pfb></usr/share/texlive/t
    exmf-dist/fonts/type1/public/amsfonts/cm/cmtt12.pfb></usr/share/texlive/texmf-d
    ist/fonts/type1/public/amsfonts/symbols/msam10.pfb></usr/share/texlive/texmf-di
    st/fonts/type1/public/amsfonts/symbols/msbm10.pfb>
    Output written on paper.pdf (285 pages, 1369913 bytes).
    PDF statistics:
     1660 PDF objects out of 1728 (max. 8388607)
     1107 compressed objects within 12 object streams
     0 named destinations out of 1000 (max. 500000)
     224 words of extra memory for PDF output out of 10000 (max. 10000000)

...do your eyes automatically skip to the line about undefined references without conscious effort? Then you need `pulp`. Compare LaTeX's spew with what `pulp` has to say about that file:

    ./delta.tex:133-133: LaTeX warning: Float too large for page by 4.64713pt on input line 133.
    ./delta.tex:2649-2649: LaTeX warning: Float too large for page by 74.48848pt on input line 2649.
    ./delta.tex:3817-3817: LaTeX warning: Float too large for page by 99.13847pt on input line 3817.
    ./delta.tex:3923-3923: wrapfig warning: Collision between wrapping environments on input line 3923.
    ./delta.tex:3925-3925: wrapfig warning: Stationary wrapfigure forced to float on input line 3925.
    ./delta.tex:3951-3951: wrapfig warning: Collision between wrapping environments on input line 3951.
    ./delta.tex:3954-3954: wrapfig warning: Stationary wrapfigure forced to float on input line 3954.
    ./skeleton_dissertation.tex:36-?: LaTeX warning: Label(s) may have changed. Rerun to get cross-references right.
    2-2: (\end occurred when \iftrue on line 2 was incomplete) 

Not only is the label warning more clearly highlighted, a number of other errors -- which were scrolled way the heck off the terminal ages ago if you were just using `pdflatex` -- are more clearly visible, together with information about which source file and line number caused the problem. (In fact, even messages which do not have a line number directly in them will have a "best guess" assigned to them by pulp to give you an idea of where in the file to start looking.)

And, just to rub in the beauty a bit more, here's a comparison, LaTeX vs. pulp, for a paper with no problems:

    % pdflatex paper
    This is pdfTeX, Version 3.1415926-2.5-1.40.14 (TeX Live 2013)
     restricted \write18 enabled.
    entering extended mode
    (./paper.tex
    LaTeX2e <2011/06/27>
    ... snipped 578 lines here ...
    public/cm-super/sfti1200.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/
    cm-super/sftt1000.pfb></usr/share/texlive/texmf-dist/fonts/type1/public/cm-supe
    r/sftt1200.pfb>
    Output written on paper.pdf (176 pages, 2075415 bytes).
    Transcript written on paper.log.
    % pulp paper.log
    %

# Example usage

To install, simply type `cabal install` from the repository directory, or `cabal install pulp` if you want the latest release from Hackage. As usual with cabal, make sure that the appropriate directory is in your `PATH`; cabal's default binary directory is `~/.cabal/bin`.

To use, compile your LaTeX file first, for example, with

    pdflatex -interaction nonstopmode foo

This should produce, among other things, a file named `foo.log`, which serves as the input to `pulp`. Use

    pulp foo.log

to parse this file and print some relevant information.

# Configuration

If you're processing `foo.log`, you can make a file `foo.log.pulp` that says what messages you're interested in. The file format is a zeroth-order logic; for example, the default configuration is

!(boring | info | message | under | over)

which blacklists "known-boring" junk, package messages at the "info" or "message" (but not "warning" or "error") level, and messages about under- and over-full boxes. Each potential output message gives a valuation of all the available atoms, and if your sentence evaluates to true with that valuation, the message gets printed. For another example, I found I was getting a bunch of warnings from a specific package; I filtered those out by tacking on:

!((warning & package 'xparse/redefine-command') | boring | info | message | under | over)

Actually, atoms only have to be spelled out enough to be unambiguous, so you could just as well write

!((w & p 'xparse/redefine-command') | b | i | m | und | o)

Later, I expect I'll want to see more under/overfull messages -- e.g. I'll want to know about boxes that are more than 10pt overfull. So I'll change the default to something like

!(b | i | m | under) & (overfull => threshold 10)

Or maybe I tried compiling with a new driver and it had some messages not caught by the standard "boring" regexen:

!(b | i | m | under | over | "^This is XeTeX, Version ")

(A regex can be enclosed by double quotes to match anywhere or single quotes to be required to match the whole text of whatever it's trying to match.) There's also all the usual logical connectives in a variety of spellings, though I've showed the common ones already, I think. The full list of atoms and a complete EBNF for the language is below.

The available atoms, and when they're true, are:

    overfull, underfull, hbox, vbox -- messages about over/underfull h/vboxes
    info, message, warning, error   -- output from \PackageError and friends
    boring          -- matches a big regex for known, boring stuff
    unknown         -- unparsed bits of the log file
    close           -- a part of the log file that looks like it should be parseable, but doesn't make sense here
    threshold <n>   -- *full *box messages whose badness/points are greater than n
    package <regex> -- \PackageError and friends was called by a package whose name matches the regex
    details <regex> -- long output from \PackageError or \ClassError matches the regex
    <regex>         -- a boring, unknown, info, message, warning, or error whose contents matches the regex
    true            -- always
    false           -- never

The configuration file format (modulo white space) is given below.

    sentence ::= chunk | chunk binop sentence

    chunk ::=
        | '(' sentence ')'
        | atom
        | nullop
        | unop chunk

    nullop ::= 'true' | 'false' | '0' | '1'
    unop   ::= 'not' | '!' | '~'
    binop  ::= '&&' | '||' | '=>' | '->' | '<=>' | '<->' | '==' | '!=' | '/=' | '^' | '*' | '+' | '/\' | '\/'
             | 'and' | 'or' | 'xor' | 'nor' | 'nand'

    regex ::= '\'' stringS '\'' | '"' stringD '"'

    stringS ::= charS*
    stringD ::= charD*

    charS ::= char | escape | '"'
    charD ::= char | escape | '\''

    char ::= anything but '"' or '\''
    escape ::= '\\n' | '\\t' | '\\"' | '\\\''

    n ::= digit+ | digit+ '.' digit+

    atom ::=
        | 'boring'
        | 'unknown'
        | 'close'
        | 'overfull'
        | 'underfull'
        | 'hbox'
        | 'vbox'
        | 'threshold' n
        | 'info'
        | 'message'
        | 'warning'
        | 'error'
        | regex
        | 'package' regex
        | 'details' regex

# Disclaimer

This tool was written by reverse-engineering the log files produced by `pdflatex` on my machine. It should work well for you if you use the same LaTeX engine, have the same version of BibTeX, use the same packages as me, have the same underlying operating system, and are in the same latitude as me. There's no guarantee it works well if anything is different. I have tried to make the parser relatively lax, and have had a few other people try it with their setups, so it's not a foregone conclusion that it *won't* work for you, but just keep your expectations realistic to begin with.

If you see something particularly distressing -- e.g. location reporting is significantly wrong, or there's something that's not easily parsed/filtered using the configuration tools already provided, you're invited to hack `lib/Text/Pulp.hs` (you might start reading at `categorize'`). You could also send along your log file to me, but I can't guarantee that I'll tackle every log file I see.
