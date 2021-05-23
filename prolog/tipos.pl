branco('\u265F', 'Peão branco').
branco('\u265C', 'Torre branca').
branco('\u265E', 'Cavalo branco').
branco('\u265D', 'Bispo branco').
branco('\u265B', 'Dama branca').
branco('\u265A', 'Rei branco').

preto('\u2659', 'Peão preto').
preto('\u2656', 'Torre preta').
preto('\u2658', 'Cavalo preto').
preto('\u2657', 'Bispo preto').
preto('\u2655', 'Dama preta').
preto('\u2654', 'Rei preto').

vazio('\u25A1', 'Espaço vazio').

pecasBrancas(R) :- R = ['\u265F','\u265C','\u265E', '\u265D', '\u265B', '\u265A'].
pecasPretas(R) :- R = ['\u2659', '\u2656', '\u2658', '\u2657', '\u2655', '\u2654'].
