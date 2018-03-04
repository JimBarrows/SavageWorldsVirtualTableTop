import fs from "fs"

class Config {

  constructor () {
    this._currentEnvironment = process.env.NODE_ENV || defaultEnvironment()
    if (this._currentEnvironment == defaultEnvironment()) {
      this._config = {
        jwt: {
          secret: process.env.JWT_SECRET || "this is an incredible secret.  the best secret in the world",
          header: process.env.JWT_HEADER || "authorization"
        },
        plotPointServer: {
          port: process.env.SERVER_PORT || 80,
          name: process.env.SERVER_NAME || "plotpointservice",
          url: process.env.SERVER_URL || "http://localhost/api/plotpointservice"
        },
        swga_db: {
          host: process.env.DATABASE_HOST || 'localhost',
          port: process.env.DATABASE_PORT || 5432,
          database: process.env.DATABASE_DATABSE || 'swga_database',
          user: process.env.DATABASE_USER || 'swga_database',
          password: process.env.DATABASE_PASSWORD || 'swga_database'
        },
        user_db: {
          host: process.env.DATABASE_HOST || 'localhost',
          port: process.env.DATABASE_PORT || 5433,
          database: process.env.DATABASE_DATABSE || 'ems_ecommerce',
          user: process.env.DATABASE_USER || 'ems_ecommerce',
          password: process.env.DATABASE_PASSWORD || 'ems_ecommerce'
        },
        userServer: {
          port: process.env.SERVER_PORT || 80,
          name: process.env.SERVER_NAME || "userservice",
          url: process.env.SERVER_URL || "http://localhost/api/user/graphql"
        },
      }
    } else {
      this._config = JSON.parse(fs.readFileSync(`config.${this.currentEnvironment}.json`, "utf8"))
    }
  }

  get currentEnvironment () {
    return this._currentEnvironment
  }

  get jwt () {
    return this._config.jwt
  }

  get swga_db () {
    return this._config.swga_db
  }

  get user_db () {
    return this._config.user_db
  }

  get plotPointServer () {
    return this._config.plotPointServer
  }

  get userServer () {
    return this._config.userServer
  }

}

const config = new Config()
export default config

export function defaultEnvironment () {
  return "default"
}

export function developmentEnvironment () {
  return "dev"
}

export function environments () {
  return [Config.defaultEnvironment(), Config.localEnvironment(), Config.developmentEnvironment(), Config.qaEnvironment(), Config.qaEnvironment(), Config.stagingEnvironment(), Config.prodEnvironment()]
}

export function localEnvironment () {
  return "local"
}

export function prodEnvironment () {
  return "prod"
}

export function qaEnvironment () {
  return "qa"
}

export function stagingEnvironment () {
  return "staging"
}
