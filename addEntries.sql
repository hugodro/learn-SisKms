insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Fri' as label, 'englishFri_200612' as locator, '2020-06-12 00:00:00' as doneOn from categories where label='english');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Fri' as label, 'mathFri_200612' as locator, '2020-06-12 00:00:00' as doneOn from categories where label='math');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Fri' as label, 'ipcFri_200612' as locator, '2020-06-12 00:00:00' as doneOn from categories where label='ipc');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Fri' as label, 'phseFri_200612' as locator, '2020-06-12 00:00:00' as doneOn from categories where label='phonics');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Moving' as label, 'phseMoving_200612' as locator, '2020-06-12 00:00:00' as doneOn from categories where label='phonics');

