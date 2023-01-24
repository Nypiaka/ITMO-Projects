CREATE TABLE post (
   id BIGINT NOT NULL AUTO_INCREMENT,
   creation_time DATETIME(6),
   text LONGTEXT,
   title VARCHAR(100),
   user_id BIGINT NOT NULL,
   PRIMARY KEY (id)
) ENGINE=InnoDB;

CREATE TABLE user (
   id BIGINT NOT NULL AUTO_INCREMENT,
   admin BIT NOT NULL,
   creation_time DATETIME(6),
   login VARCHAR(24),
   name VARCHAR(100),
   PRIMARY KEY (id)
) ENGINE=InnoDB;

ALTER TABLE user ADD CONSTRAINT UKew1hvam8uwaknuaellwhqchhb UNIQUE (login);

ALTER TABLE post
   ADD CONSTRAINT FK72mt33dhhs48hf9gcqrq4fxte 
   FOREIGN KEY (user_id) 
   REFERENCES user (id);

ALTER TABLE user ADD password_sha VARCHAR(256) NULL AFTER login;

INSERT INTO user (id, admin, creation_time, login, name, password_sha) VALUES
(1, b'0', NOW(), 'mike', 'Mike Mirzayanov', 'e6f6a4d6872ccf75e5e2cced29eb164a7c7cb604'),
(2, b'0', NOW(), 'tester', 'Tester Testerov', 'ee8ad9a2a6f0ba2cfd6b294e5d4634ddec6e9de3');

INSERT INTO post (id, creation_time, text, title, user_id) VALUES
(1, NOW(), 'Привет, Codeforces!\r\n\r\nВ понедельник, 12 декабря 2022 г. в 17:35 состоится Educational Codeforces Round 139 (Rated for Div. 2).\r\n\r\nПродолжается серия образовательных раундов в рамках инициативы Harbour.Space University! Подробности о сотрудничестве Harbour.Space University и Codeforces можно прочитать в посте.\r\n\r\nЭтот раунд будет рейтинговым для участников с рейтингом менее 2100. Соревнование будет проводиться по немного расширенным правилам ICPC. Штраф за каждую неверную посылку до посылки, являющейся полным решением, равен 10 минутам. После окончания раунда будет период времени длительностью в 12 часов, в течение которого вы можете попробовать взломать абсолютно любое решение (в том числе свое). Причем исходный код будет предоставлен не только для чтения, но и для копирования.', 'Educational Codeforces Round 139 [рейтинговый для Div. 2]', 1),
(2, NOW(), 'В этом семестре записал на видео все лекции курса \"Алгоритмы и структуры данных\", который я читаю в ИТМО. Лекции стримились в прямом эфире на твич и потом выкладывались на ютуб.\r\n\r\nКурс скорее академический, а не олимпиадный, но думаю многим начинающим (и не только) олимпиадникам тоже будет интересно. Например, эти лекции.', 'Видеолекции моего курса в ИТМО', 2),
(3, NOW(), 'Привет! В четверг, 12 декабря 2019 г. в 16:35 начнётся Codeforces Round #605 (Div. 3) — очередной Codeforces раунд для третьего дивизиона. В этом раунде будет 6 или 7 задач (или 8), которые подобраны по сложности так, чтобы составить интересное соревнование для участников с рейтингами до 1600. Однако все желающие, чей рейтинг 1600 и выше могут зарегистрироваться на раунд вне конкурса.\r\n\r\nРаунд пройдет по правилам образовательных раундов. Таким образом, во время раунда задачи будут тестироваться на предварительных тестах, а после раунда будет 12-ти часовая фаза открытых взломов. Я постарался сделать приличные тесты — так же как и вы буду расстроен, если у многих попадают решения после окончания контеста.\r\n\r\nВам будет предложено 6 или 7 (или 8) задач и 2 часа на их решение.\r\n\r\nШтраф за неверную попытку в этом раунде (и последующих Div. 3 раундах) будет равняться 10 минутам.', 'Codeforces Round #605 (Div. 3)', 1);
