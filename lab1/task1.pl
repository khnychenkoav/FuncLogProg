% my_length/2
my_length([], 0).
my_length([_|Tail], N) :- my_length(Tail, N1), N is N1 + 1.

% my_member/2
my_member(X, [X|_]).
my_member(X, [_|Tail]) :- my_member(X, Tail).

% my_append/3
my_append([], L, L).
my_append([H|T], L2, [H|L3]) :- my_append(T, L2, L3).

% my_remove/3
my_remove(X, [X|Tail], Tail).
my_remove(X, [H|Tail], [H|NewTail]) :- my_remove(X, Tail, NewTail).

% my_permute/2
my_permute([], []).
my_permute(L, [H|Perm]) :- my_remove(H, L, Rest), my_permute(Rest, Perm).

% my_sublist/2
my_sublist([], _).
my_sublist([H|T1], [H|T2]) :- my_sublist(T1, T2).
my_sublist(Sub, [_|Tail]) :- my_sublist(Sub, Tail).

% Удаление N последних элементов с использованием стандартных предикатов
remove_last_n(L, N, R) :-
    length(L, Len),       % Определить длину списка
    NewLen is Len - N,    % Вычислить новую длину списка после удаления N элементов
    length(R, NewLen),    % Создать новый список с длиной NewLen
    append(R, _, L).      % Проверить, что новый список является префиксом исходного списка

% Базовый случай: если список короче или равен N, удаляем все элементы
remove_last_n_recursive(L, N, []) :- 
    length_manual(L, Len),
    Len =< N.

% Рекурсивный случай: продолжаем удалять N последних элементов
remove_last_n_recursive([H|T], N, [H|R]) :-
    remove_last_n_recursive(T, N, R).

% Реализация length без использования стандартного предиката
length_manual([], 0).
length_manual([_|T], L) :-
    length_manual(T, L1),
    L is L1 + 1.

% Лексикографическое сравнение двух списков с использованием стандартных предикатов
lex_compare(L1, L2, Result) :-
    ( L1 @< L2 -> Result = less
    ; L1 == L2 -> Result = equal
    ; L1 @> L2 -> Result = greater
    ).

% Базовый случай: два пустых списка равны
lex_compare_recursive([], [], equal).

% Если первый список пуст, а второй непуст — первый меньше
lex_compare_recursive([], [_|_], less).

% Если второй список пуст, а первый непуст — первый больше
lex_compare_recursive([_|_], [], greater).

% Если головы списков равны, продолжаем сравнение с хвостами
lex_compare_recursive([H1|T1], [H2|T2], Result) :-
    ( H1 < H2 -> Result = less
    ; H1 > H2 -> Result = greater
    ; H1 =:= H2 -> lex_compare_recursive(T1, T2, Result)
    ).

% Предикат решает задачу сравнения двух временных рядов (списков) после удаления последних N элементов.
% У нас есть два временных ряда (списка), представляющих последовательности данных, 
% например, изменения температуры, финансовые данные, показания датчиков и т.д. Мы хотим сравнить эти два временных ряда, 
% но перед этим необходимо отбросить последние N элементов из каждого из них (например, удалить неактуальные или ошибочные данные). 
% После удаления нужно определить, какой из этих временных рядов "меньше" в лексикографическом смысле (сравнение по элементам списков).
% Если L1 меньше лексикографически, то Result = less, если больше — greater, если равны — equal.

compare_time_series_after_removal(L1, L2, N, Result) :-
    remove_last_n_recursive(L1, N, L1_trimmed),   % Удалить N последних элементов из L1
    remove_last_n_recursive(L2, N, L2_trimmed),   % Удалить N последних элементов из L2
    lex_compare_recursive(L1_trimmed, L2_trimmed, Result).  % Лексикографически сравнить списки

