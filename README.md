# ITMO-Projects
Здесь собраны и будут собираться самые сложные и значимые проекты, которые были написаны мной за время моего обучения в университете ИТМО.
**1. Scanner**
    Реализован свой аналог класса Scanner на основе Reader.
    Использовано блочное чтение. Код, управляющий чтением общий.
    Код, выделяющий числа и слова должен быть общий.
    Добавлена базовая поддержка обработки ошибок и базовые функции оригинального Scanner'а.
    Мой сканнер работает значительно быстрее при считывании больших файлов.
**2. Md to HTML парсер**
    Разработан конвертер из Markdown-разметки в HTML.
    Конвертер поддерживает следующие возможности:
    Абзацы текста разделяются пустыми строками.
    Элементы строчной разметки: выделение (* или _), сильное выделение (** или __), зачеркивание (--), код (`)
    Заголовки (# * уровень заголовка) 
Конвертер должен называется md2html.Md2Html и принимает два аргумента: название входного файла с Markdown-разметкой и название выходного файла c HTML-разметкой. Оба файла должны иметь кодировку UTF-8.
