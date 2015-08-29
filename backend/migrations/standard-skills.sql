DROP TABLE IF EXISTS "StandardSkill";
CREATE TABLE IF NOT EXISTS StandardSkill (
    "id" INTEGER ,
    "name" VARCHAR(255) NOT NULL,
    "description" VARCHAR(255) NOT NULL,
    "attribute" VARCHAR(255) NOT NULL,
    "createdAt" DATETIME NOT NULL,
    "updatedAt" DATETIME NOT NULL,    PRIMARY KEY ( "id" )

);
