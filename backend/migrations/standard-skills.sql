DROP TABLE IF EXISTS "StandardSkill";
CREATE TABLE IF NOT EXISTS StandardSkill (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "attribute" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
insert into StandardSkill('id','name','description','attribute','createdAt','updatedAt')  values(1,'Boating','Character with this skill can handle most any boat or ship moon to their setting and character backgroun. &nbsp;They generally know how to handle common tasks associated with their vessels as well ( tying knots, rigging sails, etc.).','Agility','2015-08-27 06:20:43.003 +00:00','2015-08-27 06:20:43.003 +00:00');
insert into StandardSkill('id','name','description','attribute','createdAt','updatedAt')  values(2,'Climbing','Characters may sometimes have to climb tall objects under duress, perhaps to scale a cliff to attack archers stationed above, or to evade a terrifying creature on the ground below!','Strength','2015-08-27 06:21:44.274 +00:00','2015-08-27 06:21:44.274 +00:00');
