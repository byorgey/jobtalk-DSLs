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

\newcommand{\theschool}{College University}
\newcommand{\thedate}{Februvember 43, 19823}

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
        \includegraphics[width=2in]{\sectionimg}
        \bigskip

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

\title{Building domain-specific languages and tools}
\date{\theschool \\ \thedate}
\author{Brent Yorgey}
\titlegraphic{}  % \includegraphics[width=2in]{foo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{xframe}{}
   \titlepage
%   \hfill \includegraphics[width=0.5in]{plclub}
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{My research}
  \begin{center}
    Two images here. One to represent combinatorics stuff (some
    species diagrams), other to represent DSLs, diagrams etc.
  \end{center}
\end{xframe}

\def\sectionimg{sandwich-too-full.jpg}

\section{Problems with paradigms}
\label{sec:problems}

%%
%% Suppose you wanted to produce this image.  How long would it take you?

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
%% with e.g. Inkscape/Illustrator/Gimp/Photoshop or whatever.

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

%% Some criteria for evaluating potential solutions.
%% XXX todo: center text WRT images!

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

    \includegraphics[width=2in]{Excel-barchart}
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

    % XXX TODO need image here to illustrate "flexibility"
    %\includegraphics[width=2in]{Excel-barchart}
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
      & pick it up quickly
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

\begin{xframe}{Paradigms for problem solving}
  \begin{itemize}
  \item Tools (software)
  \item General-purpose languages
  \item Domain-specific languages
  \item Embedded domain-specific languages
  \end{itemize}
\end{xframe}

%% There are some well-known software *tools* for making
%% images/diagrams like this.

\begin{xframe}{Tools}
  \begin{center}
  \includegraphics[width=1in]{illustrator_logo} \hfill
  \includegraphics[width=1in]{inkscape_logo} \hfill
  \includegraphics[width=1in]{photoshop_logo} \hfill
  \includegraphics[width=1in]{GIMP_logo} \bigskip

  \begin{overprint}
  \onslide<2>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = criteriaTable
  \end{diagram}
  \end{center}
  \onslide<3>
  \begin{center}
    \begin{diagram}[width=250]
    import Icons
    dia = drawTable
      [ [power           , happy ]
      , [flexibility     , meh   ]
      , [learning        , happy ]
      , [programmability , sad   ]
      ]
      # frame 0.5
    \end{diagram}
  \end{center}
  \end{overprint}
  \end{center}
\end{xframe}

%% Could write *programs* to generate drawings.  But using
%% general-purpose languages for this is annoying.  Spend a lot of
%% time on tedious, repetitive boilerplate that has nothing to do with
%% drawing in particular.

\begin{xframe}{General-Purpose Languages}
  \begin{center}
    XXX icons for Java, C++, etc. \bigskip

  \begin{overprint}
  \onslide<2>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = criteriaTable
  \end{diagram}
  \end{center}
  \onslide<3>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = drawTable
      [ [power           , sad   ]
      , [flexibility     , happy ]
      , [learning        , sad   ]
      , [programmability , happy ]
      ]
      # frame 0.5
    \end{diagram}
    \end{center}
  \end{overprint}
  \end{center}
\end{xframe}

%% Better to use *domain-specific* languages.

% List of downsides: these are all pretty terrible languages!  Can't
% easily combine with computation.  e.g. computing all permutations in
% order to draw them.

\begin{xframe}{Domain-specific languages (DSLs)}
  \begin{center}
    \begin{tabular}{m{1in} m{1in} m{1in} m{1in}}
      \includegraphics[width=1in]{Asymptote-logo} &
      \includegraphics[width=1in]{MPlogo.png} &
      Postscript &
      PGF/TikZ
    \end{tabular}

  \begin{overprint}
  \onslide<2>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = criteriaTable
  \end{diagram}
  \end{center}

  \onslide<3>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = drawTable
      [ [power           , happy ]
      , [flexibility     , happy ]
      , [learning        , meh   ]
      , [programmability , meh   ]
      ]
      # frame 0.5
    \end{diagram}
    \end{center}
  \end{overprint}
  \end{center}
\end{xframe}

\begin{xframe}{Embedded DSLs}
  \begin{center}
    \begin{diagram}[width=30]
      import Diagrams.Example.Logo
      dia = ico_d
    \end{diagram}

  \begin{overprint}
  \onslide<2>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = criteriaTable
  \end{diagram}
  \end{center}

  \onslide<3>
  \begin{center}
  \begin{diagram}[width=250]
    import Icons
    dia = drawTable
      [ [power           , happy ]
      , [flexibility     , happy ]
      , [learning        , meh   ]
      , [programmability , happy ]
      ]
      # frame 0.5
  \end{diagram}
  \end{center}
  \end{overprint}
  \end{center}
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \Huge{Demo!}
  \end{center}
\end{xframe}

%% XXX better image
\def\sectionimg{tree.jpg}

\section{Domain-specific languages}
\label{sec:DSLs}

\begin{xframe}{}
  \begin{center}
    What makes a good domain-specific language?
  \end{center}
\end{xframe}

\begin{xframe}{Declarative}

  \begin{center}
    \includegraphics[width=2in]{declaration}
  \end{center}

\end{xframe}

\begin{xframe}{}

\begin{center}
  How would you write code to generate this?
  \bigskip

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

\onslide<4>
\begin{center}
  \textit{Look ma, no coordinates!}
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
  \item<3-> The \emph{meaning} of the whole is determined by the
    \emph{meaning} of the parts.
  \end{itemize}

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

unit :: String -> Tree String
unit x = Node (x ++ ".o") [ Node (x ++ ".c") [] ]

t :: Tree String
t = Node "prog.exe"
    [ unit "foo"
    , unit "bar"
    , unit "baz"
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

\begin{xframe}{}
  XXX example of non-compositionality using imperative drawing
\end{xframe}

\begin{xframe}{}
  XXX show compositional diagrams again
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a matter of opinion: the best, most elegant and intuitive
%% DSLs are based on sound mathematical theory.
%%
%% Examples: 
%%
%% Diagrams: theory of monoids.  Also monoid actions.  Affine spaces.
%% Animations based on 2-categories.

\begin{xframe}{Mathematically based}
  %% XXX reword this title?
  XX
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \begin{diagram}[width=200]
      {-# LANGUAGE FlexibleInstances #-}
      import LGMDiagrams

      eqn vis a b = hcat' (with & sep .~ 1) [vis a, text "+", vis b, text "=" # named "ctr", vis (a `mappend` b)]
        # withName "ctr" (\sub -> translate (origin .-. location sub))

      newtype Next a = Next { getNext :: a }
      instance Monoid (Next (Diagram B R2)) where
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
  XXX add mentions of monoid pearl, affine space, 2-categories for animation
\end{xframe}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{xframe}{Embedded?}
  XX
\end{xframe}



\section{GUIs as programming}
\label{sec:GUIs}

%% Can we have something with happy faces across the board?

\begin{xframe}{}
  \begin{center}
    \includegraphics[width=2in]{grail} \bigskip

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

\begin{xframe}{}
  More slides here, about GUI etc.
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
