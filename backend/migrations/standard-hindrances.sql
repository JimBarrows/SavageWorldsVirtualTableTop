DROP TABLE IF EXISTS "StandardHindrance";
CREATE TABLE IF NOT EXISTS StandardHindrance (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "severity" TEXT NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
insert into StandardHindrance('id','name','severity','description','createdAt','updatedAt')  values(1,'All Thumbs','Minor','-2 Repar; Roll of 1 causes malfunction','2015-08-27 06:08:15.162 +00:00','2015-08-27 06:08:15.162 +00:00');
insert into StandardHindrance('id','name','severity','description','createdAt','updatedAt')  values(2,'Anemic','Minor','-2 to Fatigue tests','2015-08-27 06:08:37.194 +00:00','2015-08-27 06:08:37.194 +00:00');
