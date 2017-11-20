% compress basic rules.
compress([], []).
compress([Sig], [[0, Sig]]).

% if the following signal is the same
compress([Sig, Sig | SigTail], [[N2, Sig] | ComTail]) :- 
  compress([Sig | SigTail], [[N1, Sig] | ComTail]),
  succ(N1, N2),
  !.
% if the following signal is different
compress([Sig, DiffSig | SigTail], [[0, Sig], [N2, DiffSig] | ComTail]) :-
  compress([DiffSig | SigTail], [[N1, DiffSig] | ComTail]),
  succ(N1, N2),
  !.

% analyze basic rules.
analyze([], []).

% analyze 1s
analyze([[X, 1] | ComTail], ['.' | MorseTail]) :- 
  X =< 2, analyze(ComTail, MorseTail).
analyze([[X, 1] | ComTail], ['-' | MorseTail]) :- 
  X >= 2, analyze(ComTail, MorseTail).

% analyze 0s
analyze([[X, 0] | ComTail], MorseTail) :- 
  X =< 2, analyze(ComTail, MorseTail).
analyze([[X, 0] | ComTail], ['^' | MorseTail]) :- 
  X >= 2, X =< 5, analyze(ComTail, MorseTail).
analyze([[X, 0] | ComTail], ['#' | MorseTail]) :- 
  X >= 5, analyze(ComTail, MorseTail).


signal_morse([], []).
signal_morse([H | Signals], Morse) :- 
  % we need to add the double H due to how compress works.. sorry.
  compress([H, H | Signals], CompressedSignals),    
  analyze(CompressedSignals, Morse).

% converts from morse to raw message (pre-error removal)
% morse_raw/2(morse, message)
morse_raw([], []).
morse_raw(Morse, Message) :- 
  morse_raw(Morse, Message, []).

% morse_raw/3(morse, message, buffer (for recursive use))
morse_raw([], [Letter], Buffer) :- 
  morse(Letter, Buffer).
morse_raw(['^' | T], [Letter | Message], Buffer) :-
  morse(Letter, Buffer), morse_raw(T, Message, []).
morse_raw(['#' | T], ['#' | Message], []) :-
  morse_raw(T, Message, []).
morse_raw(['#' | T], [Letter, '#' | Message], Buffer) :-
  morse(Letter, Buffer),
  morse_raw(T, Message, []).
morse_raw([MorHead | MorTail], Message, Buffer) :-
  % we need append so that we can put stuff at end of list
  append(Buffer, [MorHead], NewBuffer),
  morse_raw(MorTail, Message, NewBuffer).

% remove errors
% raw_message/2(Raw, Message)
raw_message([], []).
raw_message(Raw, Message) :-
  raw_message(Raw, Message, []).

% raw_message/3(Raw, Message, Buffer (for recursive use))
raw_message([], Buffer, Buffer).
raw_message(['#' | T], NewBuffer, Buffer) :-
  append(Buffer, ['#'], Buffer2),
  raw_message(T, Message, []),
  append(Buffer2, Message, NewBuffer).
raw_message([error | T], [error | Message], []) :-
  raw_message(T, Message, []).
raw_message([error | T], Message, Buffer) :-
  \+(Buffer = []),
  raw_message(T, Message, []).
raw_message([H | T], Message, Buffer) :-
  \+(H = '#'),
  \+(H = error),
  append(Buffer, [H], NewBuffer),
  raw_message(T, Message, NewBuffer).

signal_message([], []).
signal_message(Signals, Message) :-
  signal_morse(Signals, Morse),
  morse_raw(Morse, Raw),
  raw_message(Raw, Message).


% morse dictionary (for signal_message)
morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).     % B
morse(c, [-,.,-,.]).     % C
morse(d, [-,.,.]).     % D
morse(e, [.]).       % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).     % F
morse(g, [-,-,.]).     % G
morse(h, [.,.,.,.]).     % H
morse(i, [.,.]).     % I
morse(j, [.,-,-,-]).     % J
morse(k, [-,.,-]).     % K or invitation to transmit
morse(l, [.,-,.,.]).     % L
morse(m, [-,-]).     % M
morse(n, [-,.]).     % N
morse(o, [-,-,-]).     % O
morse(p, [.,-,-,.]).     % P
morse(q, [-,-,.,-]).     % Q
morse(r, [.,-,.]).     % R
morse(s, [.,.,.]).     % S
morse(t, [-]).       % T
morse(u, [.,.,-]).     % U
morse(v, [.,.,.,-]).     % V
morse(w, [.,-,-]).     % W
morse(x, [-,.,.,-]).     % X or multiplication sign
morse(y, [-,.,-,-]).     % Y
morse(z, [-,-,.,.]).     % Z
morse(0, [-,-,-,-,-]).     % 0
morse(1, [.,-,-,-,-]).     % 1
morse(2, [.,.,-,-,-]).     % 2
morse(3, [.,.,.,-,-]).     % 3
morse(4, [.,.,.,.,-]).     % 4
morse(5, [.,.,.,.,.]).     % 5
morse(6, [-,.,.,.,.]).     % 6
morse(7, [-,-,.,.,.]).     % 7
morse(8, [-,-,-,.,.]).     % 8
morse(9, [-,-,-,-,.]).     % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)