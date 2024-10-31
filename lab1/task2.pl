:- set_prolog_flag(encoding, utf8).

:- consult('one.pl').

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
    read(Choice),  
    handle_menu_choice(Choice),
    Choice = 0, 

    format('~nПрограмма завершена.~n').

prompt_continue :-
    format('Нажмите Enter для возврата в главное меню...~n'),
    read_line_to_string(user_input, _), 
    get_char(_),
    !.

handle_menu_choice(1) :-
    format('~n--- Средний балл по группам ---~n'),
    group_average_table,  
    prompt_continue,  
    !.

handle_menu_choice(2) :-
    format('~n--- Количество должников по группам ---~n'),
    failed_students_per_group,  
    prompt_continue,  
    !.

handle_menu_choice(3) :-
    format('~n--- Студенты, не сдавшие экзамен по каждому предмету ---~n'),
    failed_students_per_subject,  
    prompt_continue, 
    !.

handle_menu_choice(4) :-
    format('~n--- Средний балл по каждому предмету ---~n'),
    subject_average_table,  
    prompt_continue,  
    !.

handle_menu_choice(5) :-
    format('~n--- Средний балл каждого студента и его статус сдачи экзаменов ---~n'),
    student_average_and_status_table,  
    prompt_continue,  
    !.

handle_menu_choice(6) :-
    format('~n--- Студенты с максимальным средним баллом по группам ---~n'),
    top_students_per_group, 
    prompt_continue, 
    !.

handle_menu_choice(0) :-
    format('Выход из программы...~n'),
    !. 

handle_menu_choice(_) :- 
    format('Неверный ввод. Попробуйте снова.~n'),
    !.


group_average_table :-
    unique_groups(Groups),  
    format('~`-t~50|~n'), 
    format('~w~t~35|~w~n', ['Group', 'Average Grade']), 
    format('~`-t~50|~n'), 
    forall(member(Group, Groups), (
        group_average_grade(Group, Average),
        format('~w~t~35|~2f~n', [Group, Average])  
    )),
    format('~`-t~50|~n'). 


failed_students_per_group :-
    unique_groups(Groups),                      
    format('~`-t~50|~n'),  
    format('~w~t~35|~w~n', ['Group', 'Failed Students']),
    format('~`-t~50|~n'),  
    forall(member(Group, Groups), (
        count_failed_students_in_group(Group, Count),
        format('~w~t~35|~w~n', [Group, Count]) 
    )),
    format('~`-t~50|~n').


failed_students_per_subject :-
    format('~`-t~100|~n'), 
    format('~w~t~40|~w~n', ['Subject', 'Failed Students']), 
    format('~`-t~100|~n'),  
    forall(subject(Code, Name), (
        students_failed_subject(Code, FailedStudents),
        format('~w (~w)~t~40|~w~n', [Name, Code, FailedStudents]) 
    )),
    format('~`-t~100|~n').  


subject_average_table :- 
    format('~`-t~60|~n'),
    format('~w~t~40|~w~n', ['Subject', 'Average Grade']), 
    format('~`-t~60|~n'), 
    forall(subject(Code, Name), (
        subject_average_grade(Code, Average),
        format('~w (~w)~t~40|~2f~n', [Name, Code, Average]) 
    )),
    format('~`-t~60|~n'). 


student_average_and_status_table :- 
    format('~`-t~60|~n'),
    format('~w~t~40|~w~n', ['Student', 'Average Grade - Status']),
    format('~`-t~60|~n'),
    forall(student(_, Student), (
        student_grades(Student, Grades),
        average_grades(Grades, Average),
        (all_exams_passed(Student) ->
            Status = 'Passed';
            Status = 'Failed'),
        format('~w~t~40|~2f - ~w~n', [Student, Average, Status]) 
    )),
    format('~`-t~60|~n').


top_students_per_group :- 
    unique_groups(Groups), 
    format('~`-t~60|~n'),  
    format('~w~t~40|~w~n', ['Group', 'Top Student(s) - Average Grade']),
    format('~`-t~60|~n'), 
    forall(member(Group, Groups), (
        top_student_in_group(Group, TopStudents, MaxAverage),
        format('~w~t~40|~w - ~2f~n', [Group, TopStudents, MaxAverage]) 
    )),
    format('~`-t~60|~n').  


unique_groups(Groups) :-
    setof(Group, Name^student(Group, Name), Groups).  

students_in_group(Group, Students) :-
    findall(Student, student(Group, Student), Students).

student_grades(Student, Grades) :-
    findall(Grade, grade(Student, _, Grade), Grades).

average_grades(Grades, Average) :-
    sum_list(Grades, Sum),
    length(Grades, Length),
    Length > 0,   
    Average is Sum / Length.

group_average_grade(Group, Average) :-
    students_in_group(Group, Students),
    findall(Grades, (member(Student, Students), student_grades(Student, Grades)), AllGrades),
    flatten(AllGrades, FlatGrades),
    average_grades(FlatGrades, Average).

subject_average_grade(SubjectCode, Average) :- 
    findall(Grade, grade(_, SubjectCode, Grade), Grades), 
    average_grades(Grades, Average).

students_failed_subject(SubjectCode, FailedStudents) :-
    findall(Student, grade(Student, SubjectCode, 2), FailedStudents).

students_failed_exam(Student) :-
    grade(Student, _, 2).

students_failed_list(FailedStudents) :-
    findall(Student, students_failed_exam(Student), FailedStudents).

count_failed_students_in_group(Group, Count) :-
    students_in_group(Group, Students),             
    students_failed_list(FailedStudents),           
    intersection(Students, FailedStudents, GroupFailedStudents),  
    length(GroupFailedStudents, Count).             

all_exams_passed(Student) :-
    \+ grade(Student, _, 2).

top_student_in_group(Group, TopStudents, MaxAverage) :- 
    students_in_group(Group, Students),
    findall(Average-Student, (member(Student, Students), student_grades(Student, Grades), average_grades(Grades, Average)), AveragesList),
    max_member(MaxAverage-_, AveragesList), 
    findall(Student, member(MaxAverage-Student, AveragesList), TopStudents).
