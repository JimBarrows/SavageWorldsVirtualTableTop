DROP TABLE IF EXISTS "StandardPowerTrapping";
CREATE TABLE IF NOT EXISTS StandardPowerTrapping (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "type" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
insert into StandardPowerTrapping('id','name','description','type','createdAt','updatedAt')  values(1,'Corrosion','If the subject with an acidic Trapping is touched the victim must make a Vigor roll or suffer Fatigue ( the equivalent of Bumps and Bruises). &nbsp;If the ability is a damaging attack that results in a Shaken or higher result, roll a d6. &nbsp;On a 6, any material the acid hit is ruined (armor loses a point of protection instead).','Acid','2015-08-27 06:11:56.063 +00:00','2015-08-27 06:11:56.063 +00:00');
insert into StandardPowerTrapping('id','name','description','type','createdAt','updatedAt')  values(2,'Burn','A power that does fixed damage reduces its die type by one, but does one less die of damage on the caster's next action unless counteracted in some way (taking an action to wash it off or stripping off the acid-covered item). &nbsp;For instance, an acid bolt causes 2d4 when hits and 2d4 on the caster's action as well (unless the target neutralizes it with liquid).','Acid','2015-08-27 06:13:36.496 +00:00','2015-08-27 06:13:36.496 +00:00');
