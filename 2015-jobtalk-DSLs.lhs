%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

%\usepackage{haskell}
%% %%include lhs2TeX-extra.fmt

%include polycode.fmt

\usepackage{brent}
\usepackage[backend=cairo,outputdir=diagrams]{diagrams-latex}
\usepackage[export]{adjustbox}
\graphicspath{{images/}}
\usepackage{ulem}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\theschool}{Grinnell College}
\newcommand{\thedate}{February 9, 2015}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\renewcommand{\onelinecomment}{\quad--- \itshape}
\renewcommand{\Varid}[1]{{\mathit{#1}}}

% \setbeamertemplate{footline}{\insertframenumber}

\setbeamertemplate{items}[circle]

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme

  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom
  \setbeamerfont{normal text}{family=\sffamily}

  % XX remove this before giving actual talk!
  % \setbeamertemplate{footline}[frame number]
  % {%
  %   \begin{beamercolorbox}{section in head/foot}
  %     \vskip2pt
  %     \hfill \insertframenumber
  %     \vskip2pt
  %   \end{beamercolorbox}
  % }

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{}

      \begin{center}
        %% \includegraphics[width=2in]{\sectionimg}
        %% \bigskip

        {\Huge \insertsectionhead}
      \end{center}
    \end{frame}
  }
}

\defbeamertemplate*{title page}{customized}[1][]
{
  \vbox{}
  \vfill
  \begin{centering}
    \begin{beamercolorbox}[sep=8pt,center,#1]{title}
      \usebeamerfont{title}\inserttitle\par%
      \ifx\insertsubtitle\@@empty%
      \else%
        \vskip0.25em%
        {\usebeamerfont{subtitle}\usebeamercolor[fg]{subtitle}\insertsubtitle\par}%
      \fi%
    \end{beamercolorbox}%
    \vskip1em\par
    {\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic\par}
    \vskip1em\par
    \begin{beamercolorbox}[sep=8pt,center,#1]{author}
      \usebeamerfont{author}\insertauthor
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{institute}
      \usebeamerfont{institute}\insertinstitute
    \end{beamercolorbox}
    \begin{beamercolorbox}[sep=8pt,center,#1]{date}
      \usebeamerfont{date}\insertdate
    \end{beamercolorbox}
  \end{centering}
  \vfill
}

\newenvironment{xframe}[1][]
  {\begin{frame}[fragile,environment=xframe,#1]}
  {\end{frame}}

% uncomment me to get 4 slides per page for printing
% \usepackage{pgfpages}
% \pgfpagesuselayout{4 on 1}[uspaper, border shrink=5mm]

% \setbeameroption{show only notes}

\renewcommand{\emph}{\textbf}

\title{Designing domain-specific languages and tools}
\date{\theschool \\ \thedate}
\author{Brent Yorgey}
\titlegraphic{\includegraphics[width=0.5in]{diagrams_logo}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

% abstract:
%
% Given a particular problem domain, there may be many ways to solve
% problems in the domain using a computer. For example, one might use
% a software tool specifically designed for solving problems in the
% domain; one might write a program in a general-purpose programming
% language such as Scheme or Java; or one might use a special-purpose,
% domain-specific programming language.  I will discuss some of the
% tradeoffs involved in this choice, particularly focusing on the use
% of domain-specific languages _embedded_ within a general-purpose
% one.  As a particular example I will demonstrate the use of
% _diagrams_, a high-level domain-specific language for creating
% vector graphics, embedded within the strongly typed functional
% programming language Haskell.

% In the second part of the talk, I will lay out a vision for
% domain-specific tools designed with tightly integrated languages,
% that make it possible to seamlessly and incrementally move back and
% forth between user interface and code.

\begin{xframe}{}
   \titlepage
%   \hfill \includegraphics[width=0.5in]{plclub}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Introduction 

%% Thanks very much for the introduction, thanks for
%% having me here.  I want to start with a challenge.

%% Suppose you wanted to produce this image.  How would you do it?
%% What sort of tools would you use, how long do you think it would
%% take you?
%% [Ask for feedback.]

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[height=150]
      import Hypothetical

      dia = perm1
    \end{diagram}
  \end{center}
\end{xframe}

%% Now how about this one?  What are the challenges?  It's repetetive,
%% but in a not entirely straightforward way.  Would be really tedious
%% with e.g. Inkscape/Illustrator/Gimp/Photoshop or whatever.  Perhaps
%% just doable.
%% [Feedback: how would you do this?  How long would it take you?]
%%
%% [See how many have taken 151 with drawing in Racket.]

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[height=150]
      import Hypothetical

      dia = perms4
    \end{diagram}
  \end{center}
\end{xframe}

%% Now what if we wanted to change all the greens to purple?

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[height=150]
      import Hypothetical
      import Control.Lens (ix)

      dia = perms4' (colors & ix 1 .~ purple)
    \end{diagram}
  \end{center}
\end{xframe}

%% Or add a bit of extra space and rotation?

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
      import Hypothetical
      import Control.Lens (ix)

      dia = perms4mod (colors & ix 1 .~ purple)
    \end{diagram}
  \end{center}
\end{xframe}

%% XXX Need to decide exactly what to say here.

%% Now, one thing you might be wondering is, how did *I* make these
%%   images??  In fact, I have a great solution to this problem: a
%%   *domain-specific language* for creating vector graphics.  XXX
%%   finish.


\begin{xframe}{Outline}
  \begin{itemize}
    \item<+-> Paradigms for problem-solving: tools and languages
    \item<+-> Embedded domain-specific languages
    \item<+-> Diagrams demo
    \item<+-> A vision for combining software tools + languages
  \end{itemize}
\end{xframe}

\section{Paradigms}
\label{sec:paradigms}

\begin{xframe}{Paradigms for problem solving}
  \begin{itemize}
  \item Software tools 
    \includegraphics[height=0.25in]{illustrator_logo}
    \includegraphics[height=0.25in]{inkscape_logo}
    \includegraphics[height=0.25in]{photoshop_logo}
    \includegraphics[height=0.25in]{GIMP_logo}
  \item General-purpose languages
    \includegraphics[height=0.25in]{Java-logo-big}
    \includegraphics[height=0.25in]{Cpp-logo}
    \includegraphics[height=0.25in]{racket-logo}
  \item Domain-specific languages
    \includegraphics[height=0.25in]{Asymptote-logo}
    \includegraphics[height=0.15in]{MPlogo}
    \includegraphics[height=0.25in]{postscript-logo}
    \includegraphics[height=0.25in]{tikz-logo}
  \item Embedded domain-specific languages
    \begin{diagram}[width=15]
      import Diagrams.Example.Logo
      dia = ico_d
    \end{diagram}
  \end{itemize}
\end{xframe}

%% Some criteria for evaluating potential solutions.

\begin{xframe}{Criteria}
  \begin{center}
    \begin{tabular}{m{1cm} l}
      \begin{diagram}[width=25,height=25]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "power") criteria # frame 0.5
      \end{diagram}
      & Power \\
      \begin{diagram}[width=25,height=25]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "flexibility") criteria # frame 0.5
      \end{diagram}
      & Flexibility \\
      \begin{diagram}[width=25,height=25]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "learning") criteria # frame 0.5
      \end{diagram}
      & Learning Curve \\
      % \begin{diagram}[width=25,height=25]    %%% Repetition falls under "programmability"
      %   import Icons
      %   import Control.Lens (ix, foldOf)
      %   dia = foldOf (ix "repetition") criteria # frame 0.5
      % \end{diagram}
      % & Repetition \\
      \begin{diagram}[width=25,height=25]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "programmability") criteria # frame 0.5
      \end{diagram}
      & Programmability \\
      % \begin{diagram}[width=25,height=25]  %%% Modifiability falls under "flexibility"
      %   import Icons
      %   import Control.Lens (ix, foldOf)
      %   dia = foldOf (ix "modification") criteria # frame 0.5
      % \end{diagram}
      % & Modifiability
    \end{tabular}
  \end{center}
\end{xframe}

\begin{xframe}{Power}
  \begin{center}
    \begin{tabular}{m{1.5cm} l}
      \begin{diagram}[width=40,height=40]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "power") criteria # frame 0.5
      \end{diagram}
      & ability to do complex things
    \end{tabular} \bigskip

    \includegraphics[width=2in]{excel-graph}
  \end{center}
\end{xframe}

\begin{xframe}{Flexibility}
  \begin{center}
    \begin{tabular}{m{1.5cm} l}
      \begin{diagram}[width=40,height=40]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "flexibility") criteria # frame 0.5
      \end{diagram}
      & ability to tweak and modify
    \end{tabular} \bigskip

    \includegraphics[width=1.5in]{excel-graph}
  \end{center}
\end{xframe}

\begin{xframe}{Learning curve}
  \begin{center}
    \begin{tabular}{m{1.5cm} l}
      \begin{diagram}[width=40,height=40]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "learning") criteria # frame 0.5
      \end{diagram}
      & how hard is it to get started?
    \end{tabular} \bigskip

    % XXX TODO need image to illustrate "learning curve"
  \end{center}
\end{xframe}

% \begin{xframe}{Repetition}
%   \begin{center}
%     \begin{tabular}{m{1.5cm} l}
%       \begin{diagram}[width=40,height=40]
%         import Icons
%         import Control.Lens (ix, foldOf)
%         dia = foldOf (ix "repetition") criteria # frame 0.5
%       \end{diagram}
%       & repetition, tedious or quick?
%     \end{tabular} \bigskip

%     % XXX TODO need image to illustrate "repetition"
%   \end{center}
% \end{xframe}

\begin{xframe}{Programmability}
  \begin{center}
    \begin{tabular}{m{1.5cm} l}
      \begin{diagram}[width=40,height=40]
        import Icons
        import Control.Lens (ix, foldOf)
        dia = foldOf (ix "programmability") criteria # frame 0.5
      \end{diagram}
      & do tedious things easily
    \end{tabular} \bigskip

    \begin{diagram}[height=100]
      import Hypothetical

      dia = perms4
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
    import Icons
    dia = drawTable (theTable # hideRows [1..4])
      # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
    import Icons
    dia = drawTable (theTable # hideRows [2..4])
      # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
    import Icons
    dia = drawTable (theTable # hideRows [3,4])
      # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
    import Icons
    dia = drawTable (theTable # hideRows [4])
      # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
    import Icons
    dia = drawTable (theTable)
      # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

%% %% There are some well-known software *tools* for making
%% %% images/diagrams like this.

%% \begin{xframe}{Software tools}
%%   \begin{center}
%%   \includegraphics[width=1in]{illustrator_logo} \hfill
%%   \includegraphics[width=1in]{inkscape_logo} \hfill
%%   \includegraphics[width=1in]{photoshop_logo} \hfill
%%   \includegraphics[width=1in]{GIMP_logo} \bigskip

%%   \begin{overprint}
%%   \onslide<2>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = criteriaTable
%%   \end{diagram}
%%   \end{center}
%%   \onslide<3>
%%   \begin{center}
%%     \begin{diagram}[width=250]
%%     import Icons
%%     dia = drawTable
%%       [ [power           , happy ]
%%       , [flexibility     , meh   ]
%%       , [learning        , happy ]
%%       , [programmability , sad   ]
%%       ]
%%       # frame 0.5
%%     \end{diagram}
%%   \end{center}
%%   \end{overprint}
%%   \end{center}
%% \end{xframe}

%% %% Could write *programs* to generate drawings.  But using
%% %% general-purpose languages for this is annoying.  Spend a lot of
%% %% time on tedious, repetitive boilerplate that has nothing to do with
%% %% drawing in particular.

%% \begin{xframe}{General-Purpose Languages}
%%   \begin{center}
%%   \includegraphics[width=0.75in]{Java-logo-big} \hfill
%%   \includegraphics[width=1in]{Cpp-logo} \hfill
%%   \includegraphics[width=1in]{racket-logo} \bigskip

%%   \begin{overprint}
%%   \onslide<2>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = criteriaTable
%%   \end{diagram}
%%   \end{center}
%%   \onslide<3>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = drawTable
%%       [ [power           , sad   ]
%%       , [flexibility     , happy ]
%%       , [learning        , sad   ]
%%       , [programmability , happy ]
%%       ]
%%       # frame 0.5
%%     \end{diagram}
%%     \end{center}
%%   \end{overprint}
%%   \end{center}
%% \end{xframe}

%% %% Better to use *domain-specific* languages.

%% % List of downsides: these are all pretty terrible languages!  Can't
%% % easily combine with computation.  e.g. computing all permutations in
%% % order to draw them.

%% \begin{xframe}{Domain-specific languages (DSLs)}
%%   \begin{center}
%%     \begin{tabular}{m{0.8in} m{0.8in} m{0.8in} m{0.8in}}
%%       \includegraphics[width=0.8in]{Asymptote-logo} &
%%       \includegraphics[width=0.8in]{MPlogo} &
%%       \includegraphics[width=0.8in]{postscript-logo} &
%%       \includegraphics[width=0.8in]{tikz-logo}
%%     \end{tabular}

%%   \begin{overprint}
%%   \onslide<2>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = criteriaTable
%%   \end{diagram}
%%   \end{center}

%%   \onslide<3>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = drawTable
%%       [ [power           , happy ]
%%       , [flexibility     , happy ]
%%       , [learning        , meh   ]
%%       , [programmability , meh   ]
%%       ]
%%       # frame 0.5
%%     \end{diagram}
%%     \end{center}
%%   \end{overprint}
%%   \end{center}
%% \end{xframe}

%% \begin{xframe}{Embedded DSLs}
%%   \begin{center}
%%     \begin{diagram}[width=30]
%%       import Diagrams.Example.Logo
%%       dia = ico_d
%%     \end{diagram}

%%   \begin{overprint}
%%   \onslide<2>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = criteriaTable
%%   \end{diagram}
%%   \end{center}

%%   \onslide<3>
%%   \begin{center}
%%   \begin{diagram}[width=250]
%%     import Icons
%%     dia = drawTable
%%       [ [power           , happy ]
%%       , [flexibility     , happy ]
%%       , [learning        , meh   ]
%%       , [programmability , happy ]
%%       ]
%%       # frame 0.5
%%   \end{diagram}
%%   \end{center}
%%   \end{overprint}
%%   \end{center}
%% \end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Domain-specific languages}
\label{sec:DSLs}

\begin{xframe}{}
  \begin{center}
    \textbf{What makes a good domain-specific language?}
  \end{center}
\end{xframe}

\begin{xframe}{Declarative}

  \begin{center}
    \includegraphics[width=2in]{declaration}
  \end{center}

\end{xframe}

\begin{xframe}{}

\begin{center}
  \begin{diagram}[width=100]
    dia = mconcat
      [ circle 5 # translateX 3 # fc blue
      , circle 5                # fc red
      ]
      # frame 0.5
  \end{diagram}
\end{center}

\begin{overprint}
\onslide<2>
First try:

\begin{center}
\begin{verbatim}
setFillColor(red);
drawCircle(5, (0,0));
setFillColor(blue);
drawCircle(5, (3,0));
\end{verbatim}
\end{center}

\onslide<3->
Better:

\begin{center}
\begin{spec}
(circle 5 # translateX 3 # fc blue) <>
(circle 5 # fc red)
\end{spec}
\end{center}
\end{overprint}

%% Notice the structure of the code reflects the structure of the
%% solution.

\onslide<4->
\begin{center}
\emph{The structure of the code reflects the structure of the solution.}
\end{center}

\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{Compositional}

\begin{center}
  \begin{diagram}[width=200]
import Data.Maybe

import Data.Tree
import Diagrams.TwoD.Layout.Tree hiding (leaf)

c1 = circle 5 # fc red
c2 = circle 5 # fc blue
c2' = c2 # translateX 3

mkNode d = d <> roundedRect 18 12 1 # fc white

leaf x = Node (mkNode x) []
node x ys = Node (mkNode x) ys

t = node (c2' <> c1) [leaf c1, node c2' [leaf c2]]

d = renderTree id (~~)
      (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                         & slHeight .~ fromMaybe (0,0) . extentY
                         & slHSep   .~ 3
                         & slVSep   .~ 3
                   )
         t
      )

dia = d # lw thin # frame 0.5
  \end{diagram}
\end{center}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{``Compositional''?}

  \begin{itemize}
  \item<2-> Build up complex things by combining simple things.
  \item<3-> \emph{The meaning of the whole is determined by the
    meaning of the parts.}
  \end{itemize}

  %% Put another way: once you have determined the meaning of the
  %% parts you don't have to think about their implementations
  %% anymore.  This is just good old abstraction.  There should also
  %% be elegant ways to combine the meanings.

\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{Examples of compositionality}
\begin{center}
\emph{Separate compilation} \bigskip

\begin{diagram}[width=200]
import Control.Arrow
import Data.List

import Diagrams.TwoD.Layout.Tree hiding (leaf)
import Data.Tree

cunit :: String -> Tree String
cunit x = Node (x ++ ".o") [ Node (x ++ ".c") [] ]

t :: Tree String
t = Node "prog.exe"
    [ cunit "foo"
    , cunit "bar"
    , cunit "baz"
    ]

dia = lw thin
    . frame 0.5
    . renderTree (\t -> text t # fc (pickColor t) # fontSizeL 0.8 <> roundedRect 6 2 0.5 # fc white) (~~)
    . (fmap . second) reflectY
    . symmLayout' (with & slWidth  .~ const (-3,3)
                        & slHeight .~ const (-1,1)
                        & slHSep   .~ 2
                        & slVSep   .~ 2
                  )
    $ t    -- $

pickColor t
  || "c" `isSuffixOf` t = blue
  || "o" `isSuffixOf` t = green
  || otherwise          = red
\end{diagram}
\end{center}
\end{xframe}

\begin{xframe}{Examples of compositionality}
\begin{center}
\emph{Unix pipes} \bigskip

\begin{verbatim}
cat foo.txt | grep 'walrus' | sort | uniq
\end{verbatim}
\end{center}
\end{xframe}

\begin{xframe}{Examples of compositionality}
  \begin{center}
    \begin{diagram}[width=100]
      dia = mconcat
        [ circle 5 # translateX 3 # fc blue
        , circle 5                # fc red
        ]
        # frame 0.5
    \end{diagram}

    \begin{spec}
      (circle 5 # translateX 3 # fc blue) <>
      (circle 5 # fc red)
    \end{spec}
  \end{center}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a matter of opinion: the best, most elegant and intuitive
%% DSLs are based on sound mathematical theory.
%%
%% Examples: 
%%
%% Diagrams: theory of monoids. Affine spaces.
%% Animations based on 2-categories.

\begin{xframe}{Mathematical foundations}
  \onslide<2-> 
  \begin{center}
    \emph{The best (domain-specific) languages are those designed with
    elegant mathematical semantics.}
  \end{center}
\end{xframe}

\begin{xframe}{Monoids}
  \begin{center}
    \begin{diagram}[width=200]
      {-# LANGUAGE FlexibleInstances #-}

      mkColor c = roundedRect 1 1 0.1 # lw none # fc c

      eqn vis a b = hcat' (with & sep .~ 1) [vis a, text "+", vis b, text "=" # named "ctr", vis (a `mappend` b)]
        # withName "ctr" (\sub -> translate (origin .-. location sub))

      newtype Next a = Next { getNext :: a }
      instance Monoid (Next (QDiagram B V2 Double Any)) where
        mempty = Next mempty
        (Next a) `mappend` (Next b) = Next (a |||||| b)

      newtype Blend a = Blend { getBlend :: a }
      instance Monoid (Blend (Colour Double)) where
        mempty = Blend white
        (Blend a) `mappend` (Blend b) = Blend (blend 0.5 a b)

      c = circle 0.5 # fc blue
      s = square 1 # fc yellow

      dia = vcat' (with & sep .~ 1)
        [ eqn id c s
        , eqn getNext (Next c) (Next s)
        , eqn (scale 0.8 . mkColor . getBlend) (Blend red) (Blend blue)
        , eqn (centerXY . strokeT)
            (fromOffsets [unitX, unitY])
            (fromOffsets [1 ^& (-1), 1 ^& 1])
        , eqn (\t -> transform t (text "F" <> square 1 # lw none)) (scalingX 3) (rotation (20 @@@@ deg))
        ]
        # frame 0.2
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=2.5in]{monoid-pearl-page1}
  \end{center}
\end{xframe}

\begin{xframe}{Affine spaces}
  \begin{center}
    \begin{diagram}[width=250]
      dia = hcat [ strutX 2, circle 0.1 # fc black, strutX 3, arrowV (4 ^& 3) # centerY ]
          # frame 0.5
    \end{diagram}

    \mbox{} \hfill $(x,y)$  \hfill $\stackrel{?}{=}$ \hfill $(x,y)$ \hfill \mbox{}
  \end{center}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{}
  \begin{center}
    \Huge{Demo!}
  \end{center}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Tools as languages}
\label{sec:GUIs}

%% Can we have something with happy faces across the board?

\begin{xframe}{}
  \begin{center}
    \onslide<2->
    \includegraphics[width=2in]{grail} \bigskip

    \onslide<1->
    \begin{diagram}[width=250]
      import Icons
      dia = drawTable
        [ [power           , happy ]
        , [flexibility     , happy ]
        , [learning        , happy ]
        , [programmability , happy ]
        ]
        # frame 0.5
    \end{diagram}
  \end{center}
\end{xframe}

%% You might think this is a pipe dream. The reason this is even
%% possible to contemplate is that the structure of the language
%% matches the structure of things being constructed.  Also,
%% compilation process that preserves provenance.

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      dia = menuBar === guiFrame (theCode # font "Courier" # scale 0.07) theDia

      theDia = 
        mconcat
        [ circle 1 # fc blue
        , mouseCursor # moveTo (3 ^& (-2))
        , circle 1.5 # fc green # translate (3 ^& (-2))
        , triangle 1 # fc yellow # translate (1 ^& (-5))
        ]

      theCode = text . unlines $
        [ "mconcat"
        , "  [ circle 1 # fc blue"
        , "  , circle 1.5 # fc green" 
        , "      # translate (3 ^& (-2))"
        , "  , triangle 1 # fc yellow"
        , "      # translate (1 ^& (-5))"
        , "  ]"
        ]

    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      dia = menuBar === guiFrame (theCode # fontSizeL 0.07 # font "Courier") theDia

      theDia = 
        mconcat
        [ circle 1 # fc blue
        , mouseCursor # moveTo newPos
        , circle 1.5 # fc green # moveTo newPos
        , circle 1.5 # fc green # translate (3 ^& (-2)) # opacity 0.2
        , triangle 1 # fc yellow # translate (1 ^& (-5))
        ]

      newPos = 2 ^& (-1)

      theCode = text . unlines $
        [ "mconcat"
        , "  [ circle 1 # fc blue"
        , "  , circle 1.5 # fc green" 
        , "      # translate (2 ^& (-1))"
        , "  , triangle 1 # fc yellow"
        , "      # translate (1 ^& (-5))"
        , "  ]"
        ]
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      
      dia =
        menuBar === circle 1 # frame 0.5 # lc red # lw thick # enframe' 4 3
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      
      dia = mconcat
        [ mouseCursor # scale 0.35 # moveTo ((-0.1) ^& (-1.2))
        , wiggleMenu
        , menuBar === circle 1 # frame 0.5 # lc red # lw thick # enframe' 4 3
        ]

      wiggleMenu = menu 0.7 ["Foozle", "Wibble", "Bazify", "Enhance", "Magic", "Wiggly"] # translateY (-0.2) # translateX (-0.6)

    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      
      dia = mconcat
        [ mouseCursor # scale 0.35 # moveTo ((-0.1) ^& (-1.2))
        , menuBar === circle 1 # wiggly 30 0.03 # strokeTrail # frame 0.5 # lc red # lw thick # enframe' 4 3 
        ]
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      import Data.Maybe

      import Data.Tree
      import Diagrams.TwoD.Layout.Tree hiding (leaf)

      dia = 
        menuBar 
        === 
        (treeView |||||| diaView) # centerX

      treeView = theTree # frame 20 # enframe
      diaView  = circle 1 # wiggly 30 0.03 # strokeTrail 
                          # frame 0.5 # lc red # lw thick # enframe

      c = ("circle 1", mempty)
      rd  = ("lc red", singl id)
      wig = ("wiggly 30 0.3", singl id)

      singl f = mconcat . map f

      mkNode (t,d) = text t # scale 2 <> d # opacity 0.2 <> roundedRect 18 12 1 # fc white

      leaf x = (snd x, Node (mkNode x) [])
      node (t,f) ys = (d, Node (mkNode (t, d)) ts)
        where
         (ds, ts) = unzip ys
         d = f ds

      t = snd $ node wig [node rd [leaf c]]  -- $

      theTree = renderTree id (~~)
            (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                               & slHeight .~ fromMaybe (0,0) . extentY
                               & slHSep   .~ 3
                               & slVSep   .~ 3
                         )
               t
            )
    \end{diagram}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=250]
      import GUI
      import Data.Maybe

      import Data.Tree
      import Diagrams.TwoD.Layout.Tree hiding (leaf)

      dia = 
        menuBar 
        === 
        ((treeView |||||| diaView) # centerX === codeView)

      treeView = theTree # frame 10 # enframe' 2 1.5
      codeView = enframe' 4 1.5 . font "Courier" . scale 0.07 . text $ unlines
        [ "wiggly :: Double -> Double -> Trail V2 Double -> Trail V2 Double"
        , "wiggly n m tr"
        , "  = cubicSpline False"
        , "  . map (\\(t, off, norm) -> origin .+^ off .+^"
        , "                            ((m * sin (tau * n * t)) *^ norm))"
        , "  . map (\\t -> (t, tr `atParam` t, tr `normalAtParam` t))"
        , "  $ [0, 0.01 .. 1]"
        ]

      diaView  = circle 1 # wiggly 30 0.03 # strokeTrail 
                          # frame 0.5 # lc red # lw thick # enframe' 2 1.5

      c = ("circle 1", mempty)
      rd  = ("lc red", singl id)
      wig = ("wiggly 30 0.3", singl id)

      singl f = mconcat . map f

      mkNode (t,d) = text t # scale 2 <> d # opacity 0.2 <> roundedRect 18 12 1 # fc white

      leaf x = (snd x, Node (mkNode x) [])
      node (t,f) ys = (d, Node (mkNode (t, d)) ts)
        where
         (ds, ts) = unzip ys
         d = f ds

      t = snd $ node wig [node rd [leaf c]]  -- $

      theTree = renderTree id (~~)
            (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                               & slHeight .~ fromMaybe (0,0) . extentY
                               & slHSep   .~ 3
                               & slVSep   .~ 3
                         )
               t
            )
    \end{diagram}
  \end{center}
\end{xframe}

%% Things to say on this slide:
%%
%% 1. Note structure of code reflects structure of solution.
%% 2. Code you see is built from simpler pieces.  Never too messy.
%% 3. ???

%% XXX Need to decide how to end.  Decide exactly what to say.

\begin{xframe}{}
  \begin{center}
    {\large Thank you!} \bigskip

    Questions?
  \end{center}
\end{xframe}

\end{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Old material


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Stuff taken from 2014 LGM talk
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%% So let me tell you a little bit about Haskell, and in particular
% %%% why Haskell makes a great host language for EDSLs.  Here are some
% %%% of the most important reasons (though there are certainly others).
% %%% I'll give some examples from diagrams to illustrate each point.

% \begin{xframe}{Haskell and EDSLs}
%   Haskell makes a great host language for DSLs:
%   \begin{itemize}
%   \item strong static type system
%   \item first-class functions
% %  \item clean syntax
%   \item powerful abstraction mechanisms
%   \item culture that encourages elegant, mathematically-based design:
%     theory meets practice
%   \end{itemize}

%   \onslide<2>
%   Full disclosure:
%   \begin{itemize}
%   \item Error messages suck
%   \end{itemize}

% \end{xframe}

% %%% The first reason Haskell makes a great host for EDSLs is its
% %%% strong static type system.  I'm sure many of you are fans of
% %%% dynamically typed languages like Python, which is a fine language.
% %%% But I often come across people who are vehemently opposed to
% %%% static typing, and from my point of view I think a large part of
% %%% that is a reaction to languages with insufficiently powerful
% %%% static type systems---those just tend to get in your way.
% %%% Haskell's type system, on the other hand, is an indispensable tool
% %%% that lets you encode properties of your programs, and then catches
% %%% large classes of bugs for you.  Examples in diagrams:

% \begin{xframe}{Types}
%   \begin{center}
%     Haskell has a \emph{strong static type system}. \bigskip

%     \onslide<2>
% \begin{minipage}{0.49\textwidth}
%   \centering
%   points
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=10]
%     dia = circle 1 # fc black # lw none
%   \end{diagram}
%   \end{tabular}
%   \medskip

%   vectors
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=30]
%     dia = arrow' (with & headLength .~ normal) 2 # rotateBy (1/10) # frame 0.3
%   \end{diagram}
%   \end{tabular}
%   \medskip

%   colors
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=30]
%     import LGMDiagrams
%     dia = mkColor red
%   \end{diagram}
%   \end{tabular}
% \end{minipage}
% \begin{minipage}{0.49\textwidth}
%   \centering
%   paths
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=50]
%     dia = cubicSpline False [origin, 2 ^& 2, 3 ^& 1, (-0.5) ^& 1, 2 ^& (-1)]
%         # frame 0.5
%   \end{diagram}
%   \end{tabular}

%   transformations
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=40]
%     dia = hcat' (with & sep .~ 1)
%       [ ltr 'F' # named (1 :: Int)
%       , ltr 'F' # shearX 0.5 # rotateBy (1/9) # reflectY # named (2 :: Int)
%       ]
%       # connectOutside' (with & gap .~ Global 0.1)
%           (1 :: Int) (2 :: Int)
%       # dashingG [0.05,0.05] 0
%       # lw thin
%     ltr x = text [x] <> square 1 # lw none
%   \end{diagram}
%   \end{tabular}

%   diagrams
%   \begin{tabular}{@@{}c@@{}}
%   \begin{diagram}[width=50]
%     dia = mconcat
%       [ circle 0.4 # fc yellow # translate ((-0.2) ^& 0.3)
%       , square 1 # fc purple # rotateBy (1/7) # translateX 0.2
%       , square 1 # fc blue
%       ]
%       # frame 0.2
%   \end{diagram}
%   \end{tabular}
% \end{minipage}
%   \medskip

%   Impossible to make silly mistakes like applying a vector to a color,
%   or adding two points.

%   \end{center}
% \end{xframe}

% %%% Haskell also has excellent support foxr first-class functions.  Can
% %%% pass them as arguments, return them, store them in data
% %%% structures.  One example of how we make use of this facility in
% %%% diagrams:  along with each diagram we store what we call an
% %%% "envelope", which you can think of as a generalization of a
% %%% bounding box.  It is a *function* which takes as input a vector
% %%% and computes how far you need to go to find the boundary.  This is
% %%% the secret sauce that allows doing things like "putting diagrams
% %%% next to each other"---along *any* axis, works just as you would
% %%% expect with transformations etc.  Ability to have functions is
% %%% really critical here; no way to store this information as
% %%% first-order data.

% \begin{xframe}{Functions}
%   \begin{center}
%     Haskell has \emph{first-class functions}.

%     \onslide<2>
%     \vfill
%     \begin{diagram}[width=250]
% mkP d = d <> square 5 # dashingG [0.1,0.1] 0
% mkD d = d <> square 5

% sepLine v d = p1 ~~ p2
%   where
%       b  = envelopeP v d
%       v' = normalized v
%       p1 = b .+^ (rotateBy (1/4) v')
%       p2 = b .+^ (rotateBy (-1/4) v')

% illustrateEnvelope v d
%   = mconcat
%     [ origin ~~ (origin .+^ v)
%       # lc black
%     , polygon (with & polyType  .~ PolyRegular 3 0.1
%                     & polyOrient .~ OrientTo (negateV v)
%               )
%       # fc black
%       # translate v
%     , sepLine v d
%       # lc blue
%     , origin ~~ envelopeP v d
%       # lc green
%     ]

% d1 = circle 1 # scaleX 2 # translate (r2 (0.5, 0.5))

% d2 = square 1.5 # translate ((-1) ^& (-1.5))

% dia = hcat [
%       (d1 # showOrigin <>
%        illustrateEnvelope (r2 (0.5,0.3)) (d1 :: D R2) <>
%        illustrateEnvelope (0.5 *^ unitY) (d1 :: D R2)
%       ) # centerY
%       , strutX 2
%       , (d2 # showOrigin' (with & oMinSize .~ 0.08) <>
%           illustrateEnvelope (normalized (3 ^& 2)) (d2 :: D R2) <>
%           illustrateEnvelope (fromDirection (13/20 @@@@ turn)) (d2 :: D R2)
%         ) # centerY
%       ]
%     # centerXY # pad 1.1
%     \end{diagram}
%   \end{center}
% \end{xframe}

% % \begin{xframe}{Syntax}
% %   XXX do I want to include this?
% % \end{xframe}

% %%% Haskell has lots of facilities for abstraction which makes it easy
% %%% to build up embedded DSLs with the desired API.
% %%%
% %%% One example of how we make use of this.  Consider a square.  You
% %%% might expect a function with a type like this: takes a side length
% %%% and constructs a diagram.
% %%%
% %%% In fact, the type of square is like this instead!  I'll explain.
% %%% We make use of Haskell's *type classes*---sort of like interfaces
% %%% in Java but more powerful.  This says square can construct *any*
% %%% type you like, as long as things of that type can be constructed
% %%% from a "trail", can be transformed, and live in 2D.  Here I have a
% %%% diagram as before, but also two paths, and a list of vertices.

% \begin{xframe}{Abstraction}
%   \begin{center}
%     Haskell has \emph{powerful abstraction mechanisms}. \medskip

%     \begin{overprint}
%     \onslide<2>
%     \begin{center}
%     \begin{diagram}[width=75]
%       dia = square 1 # frame 0.2
%     \end{diagram}

%     \begin{spec}
%          square :: Double -> Diagram
%     \end{spec}
%     \end{center}

%     \onslide<3>
%     \begin{center}
%     \begin{diagram}[width=250]
%       {-# LANGUAGE TypeFamilies #-}
%       import Data.Foldable

%       hsep :: (Monoid' a, Juxtaposable a, HasOrigin a, V a ~ R2) => Double -> [a] -> a
%       hsep n = hcat' (with & sep .~ n)

%       dot = circle 0.2 # fc blue # lw none

%       dia = hsep 1 [ square 1
%                    , (square 1 <> square 1 # reversePath # rotateBy (1/7))
%                      # stroke # fc red
%                    , square 1 # mcatmap (place dot)
%                    ]
%         # frame 0.2

%       mcatmap :: Monoid m => (a -> m) -> [a] -> m
%       mcatmap = foldMap
%     \end{diagram}
%     \end{center}

%     \begin{spec}
% square :: (TrailLike t, Transformable t, V t ~ R2)
%        => Double -> t
%     \end{spec}
%     \end{overprint}
%   \end{center}
% \end{xframe}

% %%% The last point is really about the culture of the Haskell
% %%% community, but it is reflected in the design of the language as
% %%% well.  One thing I love about the Haskell open-source community is
% %%% that it is a place where theory really meets practice---people
% %%% love talking about esoteric math and then turning around and
% %%% making it into beautiful libraries for doing everything from web
% %%% programming to databases to graphics to you name it.
% %%%
% %%% The idea is to iterate and refine your design and discover elegant
% %%% mathematical models that underlie a particular domain.
% %%% A particular example in the case of diagrams: much of it is
% %%% centered around the theory of *monoids*: idea is that things
% %%% have specific ways they can be *combined*, which has to be
% %%% "sufficiently nice".  Go through examples.

% \begin{xframe}{Design}
%   \begin{center}
%     Haskell encourages \emph{elegant, mathematically-based design}. \medskip

%     \onslide<2>
%     \begin{diagram}[width=200]
%       {-# LANGUAGE FlexibleInstances #-}
%       import LGMDiagrams

%       eqn vis a b = hcat' (with & sep .~ 1) [vis a, text "+", vis b, text "=" # named "ctr", vis (a `mappend` b)]
%         # withName "ctr" (\sub -> translate (origin .-. location sub))

%       newtype Next a = Next { getNext :: a }
%       instance Monoid (Next (Diagram B R2)) where
%         mempty = Next mempty
%         (Next a) `mappend` (Next b) = Next (a |||||| b)

%       newtype Blend a = Blend { getBlend :: a }
%       instance Monoid (Blend (Colour Double)) where
%         mempty = Blend white
%         (Blend a) `mappend` (Blend b) = Blend (blend 0.5 a b)

%       c = circle 0.5 # fc blue
%       s = square 1 # fc yellow

%       dia = vcat' (with & sep .~ 1)
%         [ eqn id c s
%         , eqn getNext (Next c) (Next s)
%         , eqn (scale 0.8 . mkColor . getBlend) (Blend red) (Blend blue)
%         , eqn (centerXY . strokeT)
%             (fromOffsets [unitX, unitY])
%             (fromOffsets [1 ^& (-1), 1 ^& 1])
%         , eqn (\t -> transform t (text "F" <> square 1 # lw none)) (scalingX 3) (rotation (20 @@@@ deg))
%         ]
%         # frame 0.2
%     \end{diagram}
%   \end{center}
% \end{xframe}

% \begin{xframe}
%   XXX add mentions of monoid pearl, 2-categories for animation
% \end{xframe}

% \section{Denotation and compositionality}
% \label{sec:denotation}

% %% TODO: What should go in this section?  Anything?  Think I need to
% %% step back a bit again and brainstorm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Old slide about the two threads to my research

%% \begin{xframe}{My research}
%%   \begin{center}
%%   \begin{tabular}{m{2in} m{2in}}
%%     \centering Combinatorics \linebreak and data types &
%%     \begin{center} Domain-specific \linebreak languages \end{center}
%%     \\
%%     \begin{center}
%%       \begin{diagram}[height=75]
%% import Diagrams.TwoD.Layout.Tree
%% import Data.Tree

%% t = nd
%%     [ nd
%%       [ nd $                    -- $
%%           leaves [B, B]
%%       , lf B
%%       ]
%%     , nd
%%       [ nd
%%         [ lf H
%%         , nd $ leaves [A, A]    -- $
%%         ]
%%       , nd $ leaves [A, A]      -- $
%%       ]
%%     ]
%%   where nd     = Node Nothing
%%         lf x   = Node (Just x) []
%%         leaves = map lf

%% data Type = A || B || H

%% drawType A = text "a" # italic # centerX <> square 2 # fc yellow
%% drawType B = text "b" # italic # centerX <> circle 1 # fc red
%% drawType H = circle 1 # fc white # dashingG [0.2,0.2] 0

%% renderT
%%   = renderTree
%%       (\x -> case x of
%%           Nothing -> mempty
%%           Just t  -> drawType t
%%       )
%%       (~~)
%%   . symmLayout' (with & slHSep .~ 4 & slVSep .~ 3)

%% dia = renderT t # frame 0.5
%%       \end{diagram}
%%     \end{center}
%%     &
%%     \begin{center}
%%     \begin{diagram}[height=75]
%% import Data.Maybe

%% import Data.Tree
%% import Diagrams.TwoD.Layout.Tree hiding (leaf)

%% c = ("circle 5", circle 5)
%% trans = ("translateX 3", singl (translateX 3))
%% rd  = ("fc red", singl (fc red))
%% bl = ("fc blue", singl (fc blue))
%% top = ("atop", mconcat)

%% singl f = mconcat . map f

%% mkNode (t,d) = text t # scale 2 <> d # opacity 0.2 <> roundedRect 18 12 1 # fc white

%% leaf x = (snd x, Node (mkNode x) [])
%% node (t,f) ys = (d, Node (mkNode (t, d)) ts)
%%   where
%%     (ds, ts) = unzip ys
%%     d = f ds

%% t = snd $ node top [node rd [leaf c], node trans [leaf c]]  -- $

%% d = renderTree id (~~)
%%       (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
%%                          & slHeight .~ fromMaybe (0,0) . extentY
%%                          & slHSep   .~ 3
%%                          & slVSep   .~ 3
%%                    )
%%          t
%%       )

%% dia = d # lw thin # frame 0.5
%%     \end{diagram}
%%     \end{center}
%%   \end{tabular}

%%   Common threads: functional programming, type systems, mathematics
%%   \end{center}
%% \end{xframe}
