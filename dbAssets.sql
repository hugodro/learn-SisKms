create table categories (
  id serial unique
  , label varchar(32)
);

create table simpleasset (
  id serial unique
  , categfk int not null references categories (id)
  , label varchar(32)
  , locator varchar(128)
  , doneOn timestamp
);


insert into categories (label) values ('english'), ('ipc'), ('math'), ('phonics'), ('pe');

-- Adding data for Monday June 8th 2020.
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Mon' as label, 'mathMon_200608' as locator, '2020-06-08 00:00:00' as doneOn from categories where label='math');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Mon' as label, 'englishMon_200608' as locator, '2020-06-08 00:00:00' as doneOn from categories where label='english');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Mon' as label, 'phsehMon_200608' as locator, '2020-06-08 00:00:00' as doneOn from categories where label='phonics');

insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Tue' as label, 'englishTue_200609' as locator, '2020-06-09 00:00:00' as doneOn from categories where label='english');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Tue' as label, 'mathTue_200609' as locator, '2020-06-09 00:00:00' as doneOn from categories where label='math');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Tue' as label, 'ipcTue_200609' as locator, '2020-06-09 00:00:00' as doneOn from categories where label='ipc');
insert into simpleasset (categfk, label, locator, doneOn)
    (select id as categfk, 'Tue' as label, 'phseTue_200609' as locator, '2020-06-09 00:00:00' as doneOn from categories where label='phonics');


