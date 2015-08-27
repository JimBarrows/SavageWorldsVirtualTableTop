DROP TABLE IF EXISTS "StandardGear";
CREATE TABLE IF NOT EXISTS StandardGear (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "era" VARCHAR(255) NOT NULL,
    "weight" INTEGER NOT NULL,
    "cost" INTEGER NOT NULL,
    "subType" VARCHAR(255) NOT NULL,
    "notes" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
insert into StandardGear('id','name','description','era','weight','cost','subType','notes','createdAt','updatedAt')  values(1,'Dagger','Short bladed weapon','Medieval',1,25,'Blades','None','2015-08-24 04:37:39.871 +00:00','2015-08-24 04:37:39.871 +00:00');
insert into StandardGear('id','name','description','era','weight','cost','subType','notes','createdAt','updatedAt')  values(2,'Great Sword','<p>2 handed big weapon.</p>','Medieval',12,400,'Blades','None','2015-08-24 04:38:10.080 +00:00','2015-08-24 04:38:10.080 +00:00');
