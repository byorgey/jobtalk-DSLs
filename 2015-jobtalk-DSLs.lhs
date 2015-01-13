%% -*- mode: LaTeX; compile-command: "mk" -*-
\documentclass[xcolor=svgnames,12pt]{beamer}

\usepackage{haskell}
%include lhs2TeX-extra.fmt

\usepackage{brent}
\usepackage[backend=cairo,outputdir=diagrams]{diagrams-latex}
\graphicspath{{images/}}
\usepackage{ulem}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\theschool}{Williams College}
\newcommand{\thedate}{January 8, 2015}

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
        \includegraphics[width=1in]{\sectionimg}
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

\begin{xframe}
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

%%
%% Suppose you wanted to produce this image.  How long would it take you?

\begin{xframe}
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

\begin{xframe}
  \begin{center}
    \begin{diagram}[height=150]
      import Hypothetical

      dia = perms4
    \end{diagram}
  \end{center}
\end{xframe}

%% Now what if we wanted to change all the greens to purple?

\begin{xframe}
  \begin{center}
    \begin{diagram}[height=150]
      import Hypothetical
      import Control.Lens (ix)

      dia = perms4' (colors & ix 1 .~ purple)
    \end{diagram}
  \end{center}
\end{xframe}

%% There are some well-known software *tools* for making
%% images/diagrams like this.

\begin{xframe}{Tools}
  \includegraphics[width=1in]{illustrator_logo} \hfill
  \includegraphics[width=1in]{inkscape_logo} \hfill
  \includegraphics[width=1in]{photoshop_logo} \hfill
  \includegraphics[width=1in]{GIMP_logo}

  \begin{minipage}[t]{2in}
  \begin{itemize}
  \item<2-> Powerful
  \item<3-> Shallow learning curve
  \end{itemize}
  \end{minipage} \hfill
  \begin{minipage}[t]{2in}
  \begin{itemize}
  \item<4-> Inflexible
  \item<5-> Tedious
  \item<6-> No computation
  \item<7-> Hard to make \emph{logical} modifications
  \end{itemize}
  \end{minipage}
\end{xframe}

%% Could write *programs* to generate drawings.  But using
%% general-purpose languages for this is annoying.  Spend a lot of
%% time on tedious, repetitive boilerplate that has nothing to do with
%% drawing in particular.

\begin{xframe}{Languages}
  XXX
\end{xframe}

%% Better to use *domain-specific* languages.

\begin{xframe}
  XXX logos for languages: Postscript, Metapost, Asymptote

  List of downsides: these are all pretty terrible languages!  Can't
  easily combine with computation.  e.g. computing all permutations in
  order to draw them.
\end{xframe}

\begin{xframe}{}
  \begin{center}
    \Huge{Demo!}
  \end{center}
\end{xframe}

\begin{xframe}{Domain-specific languages}
  foo
\end{xframe}

\def\sectionimg{tree.jpg}

\setcounter{section}{-1}
\section{Domain-specific languages}
\label{sec:DSLs}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stuff taken from 2014 LGM talk
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% So let me tell you a little bit about Haskell, and in particular
%%% why Haskell makes a great host language for EDSLs.  Here are some
%%% of the most important reasons (though there are certainly others).
%%% I'll give some examples from diagrams to illustrate each point.

\begin{xframe}{Haskell and EDSLs}
  Haskell makes a great host language for DSLs:
  \begin{itemize}
  \item strong static type system
  \item first-class functions
%  \item clean syntax
  \item powerful abstraction mechanisms
  \item culture that encourages elegant, mathematically-based design:
    theory meets practice
  \end{itemize}

  \onslide<2>
  Full disclosure:
  \begin{itemize}
  \item Error messages suck
  \end{itemize}

\end{xframe}

%%% The first reason Haskell makes a great host for EDSLs is its
%%% strong static type system.  I'm sure many of you are fans of
%%% dynamically typed languages like Python, which is a fine language.
%%% But I often come across people who are vehemently opposed to
%%% static typing, and from my point of view I think a large part of
%%% that is a reaction to languages with insufficiently powerful
%%% static type systems---those just tend to get in your way.
%%% Haskell's type system, on the other hand, is an indispensable tool
%%% that lets you encode properties of your programs, and then catches
%%% large classes of bugs for you.  Examples in diagrams:

\begin{xframe}{Types}
  \begin{center}
    Haskell has a \emph{strong static type system}. \bigskip

    \onslide<2>
\begin{minipage}{0.49\textwidth}
  \centering
  points
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=10]
    dia = circle 1 # fc black # lw none
  \end{diagram}
  \end{tabular}
  \medskip

  vectors
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=30]
    dia = arrow' (with & headLength .~ normal) 2 # rotateBy (1/10) # frame 0.3
  \end{diagram}
  \end{tabular}
  \medskip

  colors
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=30]
    import LGMDiagrams
    dia = mkColor red
  \end{diagram}
  \end{tabular}
\end{minipage}
\begin{minipage}{0.49\textwidth}
  \centering
  paths
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=50]
    dia = cubicSpline False [origin, 2 ^& 2, 3 ^& 1, (-0.5) ^& 1, 2 ^& (-1)]
        # frame 0.5
  \end{diagram}
  \end{tabular}

  transformations
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=40]
    dia = hcat' (with & sep .~ 1)
      [ ltr 'F' # named (1 :: Int)
      , ltr 'F' # shearX 0.5 # rotateBy (1/9) # reflectY # named (2 :: Int)
      ]
      # connectOutside' (with & gap .~ Global 0.1)
          (1 :: Int) (2 :: Int)
      # dashingG [0.05,0.05] 0
      # lw thin
    ltr x = text [x] <> square 1 # lw none
  \end{diagram}
  \end{tabular}

  diagrams
  \begin{tabular}{@@{}c@@{}}
  \begin{diagram}[width=50]
    dia = mconcat
      [ circle 0.4 # fc yellow # translate ((-0.2) ^& 0.3)
      , square 1 # fc purple # rotateBy (1/7) # translateX 0.2
      , square 1 # fc blue
      ]
      # frame 0.2
  \end{diagram}
  \end{tabular}
\end{minipage}
  \medskip

  Impossible to make silly mistakes like applying a vector to a color,
  or adding two points.

  \end{center}
\end{xframe}

%%% Haskell also has excellent support foxr first-class functions.  Can
%%% pass them as arguments, return them, store them in data
%%% structures.  One example of how we make use of this facility in
%%% diagrams:  along with each diagram we store what we call an
%%% "envelope", which you can think of as a generalization of a
%%% bounding box.  It is a *function* which takes as input a vector
%%% and computes how far you need to go to find the boundary.  This is
%%% the secret sauce that allows doing things like "putting diagrams
%%% next to each other"---along *any* axis, works just as you would
%%% expect with transformations etc.  Ability to have functions is
%%% really critical here; no way to store this information as
%%% first-order data.

\begin{xframe}{Functions}
  \begin{center}
    Haskell has \emph{first-class functions}.

    \onslide<2>
    \vfill
    \begin{diagram}[width=250]
mkP d = d <> square 5 # dashingG [0.1,0.1] 0
mkD d = d <> square 5

sepLine v d = p1 ~~ p2
  where
      b  = envelopeP v d
      v' = normalized v
      p1 = b .+^ (rotateBy (1/4) v')
      p2 = b .+^ (rotateBy (-1/4) v')

illustrateEnvelope v d
  = mconcat
    [ origin ~~ (origin .+^ v)
      # lc black
    , polygon (with & polyType  .~ PolyRegular 3 0.1
                    & polyOrient .~ OrientTo (negateV v)
              )
      # fc black
      # translate v
    , sepLine v d
      # lc blue
    , origin ~~ envelopeP v d
      # lc green
    ]

d1 = circle 1 # scaleX 2 # translate (r2 (0.5, 0.5))

d2 = square 1.5 # translate ((-1) ^& (-1.5))

dia = hcat [
      (d1 # showOrigin <>
       illustrateEnvelope (r2 (0.5,0.3)) (d1 :: D R2) <>
       illustrateEnvelope (0.5 *^ unitY) (d1 :: D R2)
      ) # centerY
      , strutX 2
      , (d2 # showOrigin' (with & oMinSize .~ 0.08) <>
          illustrateEnvelope (normalized (3 ^& 2)) (d2 :: D R2) <>
          illustrateEnvelope (fromDirection (13/20 @@@@ turn)) (d2 :: D R2)
        ) # centerY
      ]
    # centerXY # pad 1.1
    \end{diagram}
  \end{center}
\end{xframe}

% \begin{xframe}{Syntax}
%   XXX do I want to include this?
% \end{xframe}

%%% Haskell has lots of facilities for abstraction which makes it easy
%%% to build up embedded DSLs with the desired API.
%%%
%%% One example of how we make use of this.  Consider a square.  You
%%% might expect a function with a type like this: takes a side length
%%% and constructs a diagram.
%%%
%%% In fact, the type of square is like this instead!  I'll explain.
%%% We make use of Haskell's *type classes*---sort of like interfaces
%%% in Java but more powerful.  This says square can construct *any*
%%% type you like, as long as things of that type can be constructed
%%% from a "trail", can be transformed, and live in 2D.  Here I have a
%%% diagram as before, but also two paths, and a list of vertices.

\begin{xframe}{Abstraction}
  \begin{center}
    Haskell has \emph{powerful abstraction mechanisms}. \medskip

    \begin{overprint}
    \onslide<2>
    \begin{center}
    \begin{diagram}[width=75]
      dia = square 1 # frame 0.2
    \end{diagram}

    \begin{spec}
         square :: Double -> Diagram
    \end{spec}
    \end{center}

    \onslide<3>
    \begin{center}
    \begin{diagram}[width=250]
      {-# LANGUAGE TypeFamilies #-}
      import Data.Foldable

      hsep :: (Monoid' a, Juxtaposable a, HasOrigin a, V a ~ R2) => Double -> [a] -> a
      hsep n = hcat' (with & sep .~ n)

      dot = circle 0.2 # fc blue # lw none

      dia = hsep 1 [ square 1
                   , (square 1 <> square 1 # reversePath # rotateBy (1/7))
                     # stroke # fc red
                   , square 1 # mcatmap (place dot)
                   ]
        # frame 0.2

      mcatmap :: Monoid m => (a -> m) -> [a] -> m
      mcatmap = foldMap
    \end{diagram}
    \end{center}

    \begin{spec}
square :: (TrailLike t, Transformable t, V t ~ R2)
       => Double -> t
    \end{spec}
    \end{overprint}
  \end{center}
\end{xframe}

%%% The last point is really about the culture of the Haskell
%%% community, but it is reflected in the design of the language as
%%% well.  One thing I love about the Haskell open-source community is
%%% that it is a place where theory really meets practice---people
%%% love talking about esoteric math and then turning around and
%%% making it into beautiful libraries for doing everything from web
%%% programming to databases to graphics to you name it.
%%%
%%% The idea is to iterate and refine your design and discover elegant
%%% mathematical models that underlie a particular domain.
%%% A particular example in the case of diagrams: much of it is
%%% centered around the theory of *monoids*: idea is that things
%%% have specific ways they can be *combined*, which has to be
%%% "sufficiently nice".  Go through examples.

\begin{xframe}{Design}
  \begin{center}
    Haskell encourages \emph{elegant, mathematically-based design}. \medskip

    \onslide<2>
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

\section{Denotation and compositionality}
\label{sec:denotation}

\section{GUIs as programming}
\label{sec:GUIs}


\end{document}
