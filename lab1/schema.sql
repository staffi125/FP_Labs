SET NAMES utf8mb4;
DROP DATABASE IF EXISTS display_classes;
CREATE DATABASE display_classes CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE display_classes;

CREATE TABLE classrooms (
    id       INT AUTO_INCREMENT PRIMARY KEY,
    number   VARCHAR(20) NOT NULL UNIQUE,
    building VARCHAR(50) NOT NULL,
    seats    INT NOT NULL,
    note     VARCHAR(255) NULL
);

CREATE TABLE workstations (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id INT NOT NULL,
    inventory_no VARCHAR(30) NOT NULL UNIQUE,
    cpu          VARCHAR(60) NOT NULL,
    ram_gb       INT NOT NULL,
    os           VARCHAR(40) NOT NULL,
    is_working   TINYINT(1) NOT NULL DEFAULT 1,
    FOREIGN KEY (classroom_id) REFERENCES classrooms(id) ON DELETE CASCADE
);

CREATE TABLE teachers (
    id         INT AUTO_INCREMENT PRIMARY KEY,
    full_name  VARCHAR(150) NOT NULL,
    department VARCHAR(150) NOT NULL,
    email      VARCHAR(150) NULL
);

CREATE TABLE users (
    id         INT AUTO_INCREMENT PRIMARY KEY,
    full_name  VARCHAR(150) NOT NULL,
    category   VARCHAR(30) NOT NULL,
    group_name VARCHAR(30) NOT NULL,
    email      VARCHAR(150) NULL
);

CREATE TABLE lessons (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id INT NOT NULL,
    teacher_id   INT NOT NULL,
    subject      VARCHAR(150) NOT NULL,
    group_name   VARCHAR(30) NOT NULL,
    weekday      TINYINT NOT NULL CHECK (weekday BETWEEN 1 AND 7),
    starts_at    TIME NOT NULL,
    ends_at      TIME NOT NULL,
    FOREIGN KEY (classroom_id) REFERENCES classrooms(id) ON DELETE CASCADE,
    FOREIGN KEY (teacher_id) REFERENCES teachers(id) ON DELETE CASCADE
);

CREATE TABLE open_hours (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    classroom_id INT NOT NULL,
    weekday      TINYINT NOT NULL CHECK (weekday BETWEEN 1 AND 7),
    starts_at    TIME NOT NULL,
    ends_at      TIME NOT NULL,
    FOREIGN KEY (classroom_id) REFERENCES classrooms(id) ON DELETE CASCADE
);

CREATE TABLE sessions (
    id             INT AUTO_INCREMENT PRIMARY KEY,
    workstation_id INT NOT NULL,
    user_id        INT NOT NULL,
    session_date   DATE NOT NULL,
    starts_at      TIME NOT NULL,
    ends_at        TIME NOT NULL,
    FOREIGN KEY (workstation_id) REFERENCES workstations(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

INSERT INTO classrooms (number, building, seats, note) VALUES
    ('305', 'Корпус 2', 16, 'Проєктор, Linux і Windows'),
    ('310', 'Корпус 2', 12, NULL),
    ('41',  'Головний корпус', 20, 'Комп''ютери з GPU');

INSERT INTO workstations (classroom_id, inventory_no, cpu, ram_gb, os, is_working) VALUES
    (1, 'КБ-305-01', 'Intel Core i5-12400', 16, 'Ubuntu 24.04', 1),
    (1, 'КБ-305-02', 'Intel Core i5-12400', 16, 'Ubuntu 24.04', 1),
    (1, 'КБ-305-03', 'Intel Core i5-12400', 16, 'Windows 11', 0),
    (2, 'КБ-310-01', 'AMD Ryzen 5 5600', 16, 'Windows 11', 1),
    (2, 'КБ-310-02', 'AMD Ryzen 5 5600', 16, 'Windows 11', 1),
    (3, 'КБ-41-01', 'Intel Core i7-13700', 32, 'Ubuntu 24.04', 1),
    (3, 'КБ-41-02', 'Intel Core i7-13700', 32, 'Ubuntu 24.04', 1);

INSERT INTO teachers (full_name, department, email) VALUES
    ('Галкін Олександр Петрович', 'Кафедра інформаційних систем', NULL),
    ('Коваль Наталія Іванівна', 'Кафедра програмних систем', 'koval@knu.ua'),
    ('Литвин Сергій Андрійович', 'Кафедра теорії програмування', NULL);

INSERT INTO users (full_name, category, group_name, email) VALUES
    ('Петренко Ірина', 'студент', 'ІПС-42', NULL),
    ('Олійник Максим', 'студент', 'ІПС-41', NULL),
    ('Гнатюк Дарина', 'студент', 'ПІ-31', NULL),
    ('Савчук Віктор', 'лаборант', 'Обчислювальний центр', NULL);

INSERT INTO lessons (classroom_id, teacher_id, subject, group_name, weekday, starts_at, ends_at) VALUES
    (1, 1, 'Функціональне програмування', 'ІПС-42', 1, '08:40', '10:15'),
    (1, 1, 'Функціональне програмування', 'ІПС-41', 1, '10:35', '12:10'),
    (1, 2, 'Бази даних', 'ПІ-31', 2, '12:20', '13:55'),
    (2, 2, 'Бази даних', 'ІПС-42', 3, '08:40', '10:15'),
    (2, 3, 'Теорія алгоритмів', 'ІПС-41', 4, '10:35', '12:10'),
    (3, 3, 'Паралельні обчислення', 'ІПС-42', 5, '12:20', '15:30');

INSERT INTO open_hours (classroom_id, weekday, starts_at, ends_at) VALUES
    (1, 1, '14:00', '18:00'), (1, 3, '10:00', '18:00'),
    (2, 2, '09:00', '17:00'), (3, 6, '10:00', '15:00');

INSERT INTO sessions (workstation_id, user_id, session_date, starts_at, ends_at) VALUES
    (1, 1, '2026-10-26', '14:10', '16:00'), (2, 2, '2026-10-26', '14:30', '15:45'),
    (1, 3, '2026-10-28', '10:15', '12:00'), (4, 1, '2026-10-27', '09:30', '11:30'),
    (5, 2, '2026-10-27', '13:00', '14:20'), (6, 1, '2026-10-31', '10:00', '13:00'),
    (1, 2, '2026-10-28', '15:00', '17:30');
