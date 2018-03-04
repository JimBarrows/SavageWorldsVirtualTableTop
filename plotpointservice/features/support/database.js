var pgp = require('pg-promise')()
import config from "./config"

const swga_db = pgp(config.swga_db)
const user_db = pgp(config.user_db)
export {swga_db, user_db}
