DROP TABLE IF EXISTS "StandardPowerTrapping";
CREATE TABLE IF NOT EXISTS StandardPowerTrapping (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "type" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
