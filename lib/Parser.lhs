\section{Web frontend for the model checker}\label{sec:Web}

To enhance the usability of the BSML model checker, we have developed a web-based interface using Haskell for the backend and various modern web technologies for the frontend. The web application allows users to input modal logic formulas, visualize Kripke models, and dynamically view verification results. The process works as follows:

We implemented the frontend using Next.js, KaTeX \cite{katex}, and HTML5 Canvas \cite{html5_canvas}. Users can enter models, states, and formulas through input fields, and the web application submits model-checking queries via HTTP requests to the backend.

On the backend, we use Scotty, a lightweight Haskell web framework, to handle requests from the frontend and run the model checker. Once the computation is complete, the backend returns the verification result (True/False) to the frontend.

Additionally, the frontend generates a graph representation of the Kripke model and states, providing users with a visual understanding of the verification process.

This web server makes the BSML model checker more accessible and user-friendly, allowing users to verify modal logic formulas without writing any Haskell code.

The web server's source code is available on GitHub \cite{project_web}. 
We developed this project with assistance from \textbf{V0 AI} \cite{v0_dev}, and detailed prompt information can be found in the README.


\subsection{Web-Based User Interface}
We developed a Next.js frontend that provides an intuitive user interface for building and evaluating logical models. 
To handle mathematical formulas, we integrated KaTeX, a JavaScript library, which dynamically renders user-entered LaTeX formulas into HTML for clear and precise display. 
For visualizing Kripke models, we utilized HTML5 Canvas to dynamically draw the worlds (nodes) and relationships (edges) of the logical model. 
The nodes are color-coded to represent different states, and the graph updates in real-time based on user input, providing an interactive and responsive experience.

To facilitate communication with the Haskell backend, we defined a structured interface, ModelEvaluationRequest, which encapsulates the essential elements of Kripke models and logical formulas. 
This interface includes:

\begin{itemize}
\item universe: A list of world identifiers.
\item valuation: A mapping of worlds to the propositions that hold true in them.
\item relation: A list of relationships (edges) between worlds.
\item state: The selected states (worlds) for evaluation.
\item formula: The logical formula to be evaluated.
\item isSupport: A boolean value, true means support(|=), false means not support (=|).
\end{itemize}

The frontend sends this data as a POST request to the backend, enabling seamless evaluation and retrieval of results. 


\subsection{Formula Evaluation}
The Parser module is responsible for parsing logical formulas into the internal BSMLForm representation. It supporrs:
\begin{itemize}
    \item \textbf{Atomic propositions}: e.g., \( p_1, p_2 \)
    \item \textbf{Negation}: \( ! \) (not)
    \item \textbf{Conjunction}: \( \& \)
    \item \textbf{Disjunction}: \( | \)
    \item \textbf{Global disjunction}: \( / \)
    \item \textbf{Diamond} \(\lozenge\)
\end{itemize}

\begin{code}
module Parser where

import Syntax
import Text.Parsec

-- Based on the Parsec Homework
pForm :: Parsec String () BSMLForm
pForm = spaces >> pCnt <* (spaces >> eof) where
  pCnt =  chainl1 pDiaBox (spaces >> (pGdis <|> pDisj <|> pConj))
  
  pConj = char '&' >> return Con
  pDisj = char '|' >> return Dis
  pGdis = char '/' >> return Gdis

  -- Diamond operator has higher precedence than conjunction
  pDiaBox = spaces >> ( try (pDiaOp <|> pBoxOp) <|> pAtom)
  pDiaOp = char '<' >> char '>' >> Dia <$> pDiaBox
  pBoxOp = char '[' >> char ']' >> box <$> pDiaBox
  
  -- An atom is a variable, negation, or a parenthesized formula
  pAtom = spaces >> (pBot <|> pNE <|> pVar <|> pNeg <|> (spaces >> char '(' *> pCnt <* char ')' <* spaces))
  
  -- A variable is 'p' followed by digits
  pVar = char 'p' >> P . read <$> many1 digit <* spaces
  
  pBot = string "bot" >> return Bot
  pNE = string "ne" >> return NE

  -- A negation is '!' followed by an atom

  pNeg = char '!' >> Neg <$> pDiaBox

parseForm :: String -> Either ParseError BSMLForm
parseForm = parse pForm "input"

parseForm' :: String -> BSMLForm
parseForm' s = case parseForm s of
  Left e -> error $ show e
  Right f -> f

\end{code}