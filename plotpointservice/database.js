import config from "./config"

const pgp = require('pg-promise')();

export default pgp(config.database);