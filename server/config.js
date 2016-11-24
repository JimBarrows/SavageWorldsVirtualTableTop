import fs from "fs";

class Config {

	constructor() {
		this._currentEnvironment = process.env.ENV || defaultEnvironment();
		this._config             = {
			server: {
				port: 3000,
				sessionKeys: ["superDoubleSecretKey1", "superTripleSecrektKey2", "supercalifragilisticexpialidocious"],
				viewEngine: "pug",
				logLevel: "dev"
			},
			mongoose: {
				url: "mongodb://localhost/FlashCards"
			},
			jwt: {
				secret: "this is an incredible secret.  the best secret in the world",
				header: "x-access-token"
			}
		};
		if (this._currentEnvironment !== defaultEnvironment()) {
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

export function defaultEnvironment() {
	return "default";
}

export function developmentEnvironment() {
	return "dev";
}

export function environments() {
	return [Config.defaultEnvironment(), Config.localEnvironment(), Config.developmentEnvironment(), Config.qaEnvironment(), Config.qaEnvironment(), Config.stagingEnvironment(), Config.prodEnvironment()];
}

export function localEnvironment() {
	return "local";
}

export function prodEnvironment() {
	return "prod";
}

export function qaEnvironment() {
	return "qa";
}

export function stagingEnvironment() {
	return "staging";
}