import fs from "fs"

class Config {

  constructor () {
    this._currentEnvironment = process.env.NODE_ENV || defaultEnvironment()
    if (this._currentEnvironment == defaultEnvironment()) {
      this._config = {
        database: {
          host: process.env.DATABASE_HOST || 'swga_database',
          port: process.env.DATABASE_PORT || 5432,
          database: process.env.DATABASE_DATABSE || 'swga_database',
          user: process.env.DATABASE_USER || 'swga_database',
          password: process.env.DATABASE_PASSWORD || 'swga_database'
        },
        server: {
          cors: process.env.SERVER_CORS || null,
          endpoint: process.env.SERVER_ENDPOINT || '/',
          name: process.env.SERVER_NAME || "Plot Point Service",
          playground: process.env.SERVER_PLAYGROUND || '/',
          port: process.env.SERVER_PORT || 4000,
          subscriptions: process.env.SERVER_SUBSCRIPTIONS || '/',
          tracing: process.env.SERVER_TRACING || false,
          uploads: process.env.SERVER_UPLOADS || false,
          url: process.env.SERVER_URL || "http://localhost"
        },
        jwt: {
          secret: process.env.JWT_SECRET || "this is an incredible secret.  the best secret in the world",
          header: process.env.JWT_HEADER || "authorization"
        }
      }
    } else {
      this._config = JSON.parse(fs.readFileSync(`config.${ this.currentEnvironment }.json`, "utf8"))
    }
  }

  get currentEnvironment () {
    return this._currentEnvironment
  }

  get database () {
    return this._config.database
  }

  get graphql () {
    return this._config.graphql
  }

  get jwt () {
    return this._config.jwt
  }

  get server () {
    return this._config.server
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
  return [
    Config.defaultEnvironment(),
    Config.localEnvironment(),
    Config.developmentEnvironment(),
    Config.qaEnvironment(),
    Config.qaEnvironment(),
    Config.stagingEnvironment(),
    Config.prodEnvironment()
  ]
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
