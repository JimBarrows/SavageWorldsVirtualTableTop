import fs from "fs";

class Config {

	constructor() {
		this._currentEnvironment = process.env.ENV || defaultEnvironment;
		this._config             = {
			server: {
				port: 3000,
				sessionKeys: ["superDoubleSecretKey1", "superTripleSecrektKey2", "supercalifragilisticexpialidocious"],
				viewEngine: "pug",
				logLevel: "dev"
			},
			mongoose: {
				url: "mongodb://localhost/swvtt"
			},
			jwt: {
				secret: "this is an incredible secret.  a big league secret. big league",
				header: "x-access-token"
			}
		};
		if (this._currentEnvironment !== defaultEnvironment) {
			_config = JSON.parse(fs.readFileSync(`config.${this.currentEnvironment}.json`, "utf8"));
		}
	}

	get currentEnvironment() {
		return this._currentEnvironment;
	}

	get jwt() {
		return this._config.jwt;
	}

	get mongoose() {
		return this._config.mongoose;
	}

	get server() {
		return this._config.server;
	}

}

const config = new Config();
export default config;

export const defaultEnvironment = "default";

export const developmentEnvironment = "dev";

export const environments = [Config.defaultEnvironment, Config.localEnvironment, Config.developmentEnvironment, Config.qaEnvironment, Config.qaEnvironment, Config.stagingEnvironment, Config.prodEnvironment];

export const localEnvironment = "local";

export const prodEnvironment = "prod";

export const qaEnvironment = "qa";

export const stagingEnvironment = "staging";