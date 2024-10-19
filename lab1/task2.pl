% Устанавливаем кодировку UTF-8
:- set_prolog_flag(encoding, utf8).

% Подключаем файл с данными
:- consult('one.pl').

% --- Основное интерактивное меню ---
main_menu :-
    repeat,
    format('~n--- Main Menu ---~n'),
    format('1. Показать средний балл по группам~n'),
    format('2. Показать количество должников по группам~n'),
    format('3. Показать студентов, не сдавших экзамен по каждому предмету~n'),
    format('4. Показать средний балл по каждому предмету~n'),
    format('5. Показать средний балл каждого студента и его статус сдачи экзаменов~n'),
    format('6. Показать студентов с максимальным средним баллом по группам~n'),
    format('0. Выйти~n'),
    format('Введите номер действия: '),
    read(Choice),  % Считываем выбор пользователя
    handle_menu_choice(Choice),
    Choice = 0,  % Условие завершения - если выбран "0" (выход)

    format('~nПрограмма завершена.~n').

prompt_continue :-
    format('Нажмите Enter для возврата в главное меню...~n'),
    read_line_to_string(user_input, _),  % Ожидание нажатия Enter
    get_char(_),
    !.

% --- Обработчик выбора пользователя ---
handle_menu_choice(1) :-
    format('~n--- Средний балл по группам ---~n'),
    group_average_table,  % Вызов предиката для отображения среднего балла по группам
    prompt_continue,  % Запрос на продолжение
    !.  % Успешный выход, возвращаемся в главное меню

handle_menu_choice(2) :-
    format('~n--- Количество должников по группам ---~n'),
    failed_students_per_group,  % Вызов предиката для отображения количества должников
    prompt_continue,  % Запрос на продолжение
    !.

handle_menu_choice(3) :-
    format('~n--- Студенты, не сдавшие экзамен по каждому предмету ---~n'),
    failed_students_per_subject,  % Вызов предиката для отображения студентов, не сдавших экзамены
    prompt_continue,  % Запрос на продолжение
    !.

handle_menu_choice(4) :-
    format('~n--- Средний балл по каждому предмету ---~n'),
    subject_average_table,  % Вызов предиката для отображения среднего балла по каждому предмету
    prompt_continue,  % Запрос на продолжение
    !.

handle_menu_choice(5) :-
    format('~n--- Средний балл каждого студента и его статус сдачи экзаменов ---~n'),
    student_average_and_status_table,  % Вызов предиката для отображения среднего балла каждого студента
    prompt_continue,  % Запрос на продолжение
    !.

handle_menu_choice(6) :-
    format('~n--- Студенты с максимальным средним баллом по группам ---~n'),
    top_students_per_group,  % Вызов предиката для отображения студентов с максимальным баллом
    prompt_continue,  % Запрос на продолжение
    !.

handle_menu_choice(0) :-
    format('Выход из программы...~n'),
    !.  % Завершаем выполнение программы

handle_menu_choice(_) :-  % Обработка неверного выбора
    format('Неверный ввод. Попробуйте снова.~n'),
    !.

% --- Таблица среднего балла по группам ---

% Для каждой группы считаем средний балл и выводим в виде таблицы
group_average_table :-
    unique_groups(Groups),  % Получаем список уникальных групп
    format('~`-t~50|~n'),  % Линия-разделитель
    format('~w~t~35|~w~n', ['Group', 'Average Grade']),  % Заголовки
    format('~`-t~50|~n'),  % Линия-разделитель
    forall(member(Group, Groups), (
        group_average_grade(Group, Average),
        format('~w~t~35|~2f~n', [Group, Average])  % Вывод строки таблицы
    )),
    format('~`-t~50|~n').  % Линия-разделитель

% --- Таблица количества должников по группам ---

% Подсчитываем количество должников в группе и выводим в виде таблицы
failed_students_per_group :-
    unique_groups(Groups),                           % Собираем уникальные группы
    format('~`-t~50|~n'),  % Линия-разделитель
    format('~w~t~35|~w~n', ['Group', 'Failed Students']),  % Заголовки
    format('~`-t~50|~n'),  % Линия-разделитель
    forall(member(Group, Groups), (
        count_failed_students_in_group(Group, Count),
        format('~w~t~35|~w~n', [Group, Count])  % Вывод строки таблицы
    )),
    format('~`-t~50|~n').  % Линия-разделитель

% --- Таблица студентов, не сдавших экзамен по каждому предмету ---

% Для каждого предмета выводим список студентов, не сдавших экзамен, в табличной форме
failed_students_per_subject :-
    format('~`-t~100|~n'),  % Линия-разделитель
    format('~w~t~40|~w~n', ['Subject', 'Failed Students']),  % Заголовки
    format('~`-t~100|~n'),  % Линия-разделитель
    forall(subject(Code, Name), (
        students_failed_subject(Code, FailedStudents),
        format('~w (~w)~t~40|~w~n', [Name, Code, FailedStudents])  % Вывод строки таблицы
    )),
    format('~`-t~100|~n').  % Линия-разделитель

% --- Таблица среднего балла по каждому предмету ---

% Для каждого предмета выводим средний балл
subject_average_table :- 
    format('~`-t~60|~n'),  % Линия-разделитель
    format('~w~t~40|~w~n', ['Subject', 'Average Grade']),  % Заголовки
    format('~`-t~60|~n'),  % Линия-разделитель
    forall(subject(Code, Name), (
        subject_average_grade(Code, Average),
        format('~w (~w)~t~40|~2f~n', [Name, Code, Average])  % Вывод строки таблицы
    )),
    format('~`-t~60|~n').  % Линия-разделитель

% --- Таблица среднего балла для каждого студента и проверка сдачи экзамена ---

% Для каждого студента находим средний балл и проверяем, сдал ли он все экзамены
student_average_and_status_table :- 
    format('~`-t~60|~n'),  % Линия-разделитель
    format('~w~t~40|~w~n', ['Student', 'Average Grade - Status']),  % Заголовки
    format('~`-t~60|~n'),  % Линия-разделитель
    forall(student(_, Student), (
        student_grades(Student, Grades),
        average_grades(Grades, Average),
        (all_exams_passed(Student) ->
            Status = 'Passed';
            Status = 'Failed'),
        format('~w~t~40|~2f - ~w~n', [Student, Average, Status])  % Вывод строки таблицы
    )),
    format('~`-t~60|~n').  % Линия-разделитель

% --- Таблица студентов с максимальным средним баллом в группе ---

% Для каждой группы находим студентов с максимальным средним баллом
top_students_per_group :- 
    unique_groups(Groups), 
    format('~`-t~60|~n'),  % Линия-разделитель
    format('~w~t~40|~w~n', ['Group', 'Top Student(s) - Average Grade']),  % Заголовки
    format('~`-t~60|~n'),  % Линия-разделитель
    forall(member(Group, Groups), (
        top_student_in_group(Group, TopStudents, MaxAverage),
        format('~w~t~40|~w - ~2f~n', [Group, TopStudents, MaxAverage])  % Вывод строки таблицы
    )),
    format('~`-t~60|~n').  % Линия-разделитель

% --- Вспомогательные предикаты ---

% Собираем уникальные группы студентов
unique_groups(Groups) :-
    setof(Group, Name^student(Group, Name), Groups).  % Собираем уникальные группы

% Находим всех студентов в группе
students_in_group(Group, Students) :-
    findall(Student, student(Group, Student), Students).

% Находим все оценки студента
student_grades(Student, Grades) :-
    findall(Grade, grade(Student, _, Grade), Grades).

% Подсчет среднего балла для списка оценок
average_grades(Grades, Average) :-
    sum_list(Grades, Sum),
    length(Grades, Length),
    Length > 0,   % Чтобы избежать деления на 0
    Average is Sum / Length.

% Подсчет среднего балла для группы
group_average_grade(Group, Average) :-
    students_in_group(Group, Students),
    findall(Grades, (member(Student, Students), student_grades(Student, Grades)), AllGrades),
    flatten(AllGrades, FlatGrades), % Собираем все оценки в один список
    average_grades(FlatGrades, Average).

% Подсчет среднего балла для предмета
subject_average_grade(SubjectCode, Average) :- 
    findall(Grade, grade(_, SubjectCode, Grade), Grades), 
    average_grades(Grades, Average).

% Находим студентов, не сдавших экзамен по предмету
students_failed_subject(SubjectCode, FailedStudents) :-
    findall(Student, grade(Student, SubjectCode, 2), FailedStudents).

% Находим всех студентов, не сдавших хотя бы один экзамен
students_failed_exam(Student) :-
    grade(Student, _, 2).

% Получаем список всех студентов, не сдавших хотя бы один экзамен
students_failed_list(FailedStudents) :-
    findall(Student, students_failed_exam(Student), FailedStudents).

% Подсчитываем количество должников в группе
count_failed_students_in_group(Group, Count) :-
    students_in_group(Group, Students),              % Находим студентов в группе
    students_failed_list(FailedStudents),            % Находим всех должников
    intersection(Students, FailedStudents, GroupFailedStudents),  % Студенты группы, не сдавшие экзамен
    length(GroupFailedStudents, Count).              % Подсчитываем их количество

% Проверяем, сдал ли студент все экзамены (нет оценок 2)
all_exams_passed(Student) :-
    \+ grade(Student, _, 2).

% Находим студента с максимальным средним баллом в группе
top_student_in_group(Group, TopStudents, MaxAverage) :- 
    students_in_group(Group, Students),
    findall(Average-Student, (member(Student, Students), student_grades(Student, Grades), average_grades(Grades, Average)), AveragesList),
    max_member(MaxAverage-_, AveragesList),  % Находим максимальный средний балл
    findall(Student, member(MaxAverage-Student, AveragesList), TopStudents).  % Находим всех студентов с этим средним баллом
