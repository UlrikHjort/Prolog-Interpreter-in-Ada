% Database Query Examples
% Simulating relational database operations in Prolog.

% Employee database
% employee(ID, Name, Department, Salary)
employee(1, john, engineering, 75000).
employee(2, mary, engineering, 82000).
employee(3, bob, sales, 65000).
employee(4, alice, marketing, 70000).
employee(5, charlie, engineering, 90000).
employee(6, diana, sales, 72000).
employee(7, eve, marketing, 68000).
employee(8, frank, engineering, 78000).

% Department database
% department(Name, Manager, Budget)
department(engineering, charlie, 500000).
department(sales, bob, 300000).
department(marketing, alice, 250000).

% Project database
% project(ID, Name, Department, Budget)
project(101, alpha, engineering, 100000).
project(102, beta, engineering, 150000).
project(103, gamma, sales, 80000).
project(104, delta, marketing, 60000).

% Basic queries
% Find all employees in a department
employees_in_dept(Dept, Employees) :-
    findall(Name, employee(_, Name, Dept, _), Employees).

% Find employees earning more than X
high_earners(MinSalary, Employees) :-
    findall(Name, (employee(_, Name, _, Sal), Sal > MinSalary), Employees).

% Manual findall implementation
findall(Term, Goal, List) :-
    collect(Term, Goal, [], List).

collect(Term, Goal, Acc, List) :-
    copy_term((Term, Goal), (TermCopy, GoalCopy)),
    GoalCopy,
    \+ member(TermCopy, Acc), !,
    collect(Term, Goal, [TermCopy|Acc], List).
collect(_, _, List, List).

% Calculate total salary for department
dept_total_salary(Dept, Total) :-
    findall(Sal, employee(_, _, Dept, Sal), Salaries),
    sum_list(Salaries, Total).

sum_list([], 0).
sum_list([H|T], S) :- sum_list(T, S1), S is H + S1.

% Average salary in department
dept_avg_salary(Dept, Avg) :-
    findall(Sal, employee(_, _, Dept, Sal), Salaries),
    sum_list(Salaries, Total),
    length(Salaries, Count),
    Count > 0,
    Avg is Total / Count.

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Join: Find employee details with their department info
employee_dept_info(Name, Dept, Salary, Manager, Budget) :-
    employee(_, Name, Dept, Salary),
    department(Dept, Manager, Budget).

% Find the highest paid employee
max_salary_employee(Name, Salary) :-
    employee(_, Name, _, Salary),
    \+ (employee(_, _, _, Sal2), Sal2 > Salary).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Example queries:
% ?- employee(_, Name, engineering, Salary).
% ?- max_salary_employee(Name, Salary).
% ?- employee_dept_info(john, Dept, Salary, Manager, Budget).
