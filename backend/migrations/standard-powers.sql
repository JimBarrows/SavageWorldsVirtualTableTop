DROP TABLE IF EXISTS "StandardPower";
CREATE TABLE IF NOT EXISTS StandardPower (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "rank" VARCHAR(255) NOT NULL,
    "powerPoints" INTEGER NOT NULL,
    "range" VARCHAR(255) ,
    "duration" INTEGER NOT NULL,
    "maintenanceCost" INTEGER NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
insert into StandardPower('id','name','description','rank','powerPoints','range','duration','maintenanceCost','createdAt','updatedAt')  values(1,'Armor','<p>Armor creates a field of magical protection around a character or an actual shell of some sort, effectively given the target Armor. &nbsp;Success grants the recipient 2 points of Armor. &nbsp;A raise grants 4 points of Armor.</p><p>Whether the armor is visible or not depends largely on the trapping.</p>','Novice',2,'Touch',3,1,'2015-08-27 06:15:52.756 +00:00','2015-08-27 06:15:52.756 +00:00');
insert into StandardPower('id','name','description','rank','powerPoints','range','duration','maintenanceCost','createdAt','updatedAt')  values(2,'Banish','<p>Whether ghosts, elementals or demons, banish removes them all. &nbsp;This power can affect any creature that is not native to the current plane of existence (GM's determination).</p><p>This spell is an opposed roll of the caster's arcane skill versus the target's Spirit. &nbsp;On a success, the target is Shaken. &nbsp;On a raise, it is sent to it's proper plane of existence.</p><p>If the target is a Wild Card, each casting o banish causes a wound instead. &nbsp;If the target already has three wounds it is then banished to its native plane - but it is not slain.</p>','Veteran',3,'Smarts',0,0,'2015-08-27 06:18:58.304 +00:00','2015-08-27 06:18:58.304 +00:00');
