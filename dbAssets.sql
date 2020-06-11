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
-- insert into simpleasset (categfk, label, locator, doneOn)
--     (select id as categfk, 'Wed' as label, 'englishWed_200610' as locator, '2020-06-10 00:00:00' as doneOn from categories where label='english');
-- insert into simpleasset (categfk, label, locator, doneOn)
--     (select id as categfk, 'Wed' as label, 'mathWed_200610' as locator, '2020-06-10 00:00:00' as doneOn from categories where label='math');
-- insert into simpleasset (categfk, label, locator, doneOn)
--     (select id as categfk, 'Wed' as label, 'ipcWed_200610' as locator, '2020-06-10 00:00:00' as doneOn from categories where label='ipc');
-- insert into simpleasset (categfk, label, locator, doneOn)
--     (select id as categfk, 'Wed' as label, 'phseWed_200610' as locator, '2020-06-10 00:00:00' as doneOn from categories where label='phonics');


