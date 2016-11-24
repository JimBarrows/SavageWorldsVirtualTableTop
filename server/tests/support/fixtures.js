import Account from "../../models/account";
import axios from "axios";
import bluebird from "bluebird";
import chai from "chai";
import chaiAsPromised from "chai-as-promised";
import chaiThings from "chai-things";


chai.use(chaiAsPromised);
chai.use(chaiThings);

export const client = createAxiosClient();

export const expect = chai.expect;

export const Promise = bluebird;

export const user = {
	username: "chester@tester.com"
	, password: "thisisthepassword"
};

export const hash = "0933f0cfbd7916d673c9d88ca9b1bd31c41705f7b22bf475bbebc178feb6dab05a87a4d37cade0422c97d28f93daa3e19a1edbff260e063a28bfd39db8fff27a289bbfdde4f9455eec526b05950f190f215bc42d1d1232b70f8a397a652e995aa5743194bf442dc2388fd67ebbd284f5db05e2a49ccf944f060dcc914c4e9f8fd9e135c6b3d274cda05429568baeac0c38938b585ba3ff45506739719d057bd0f2f2a32fa807a6350395f79aee9342603e512d379c27d9e30418d48913be8def133b4a6af3963ac4277d2faca982894f1afb4a35aab43c03bccd5c055dd91ff3699c380e23cf708d064cba6a793cde8f87cd2479cc8ab070001b36274e85bc9b3c6c75697ca41262db5c29a77f08d5ff1bcdc24e40e983999ef69341aaa3fed8d4376bf02f5395f650a02ddc1e16f700522af03b9c2a8115eff32420563c98e7f33d67b4bef159a879920e7939300c676f9295a79492ea38821379a2e90ddb1de66c36f4f56c9504261a834d186efc1cfd65c9918fa565b92e4b56abc97c559786962e1779660980792985d29234d42ebb4fab5f7b41a6f6f1779885234fca68461f21f0772fa1e8da9a23a3f5fe839afe861f8decbe0b456115c5abf010851fe12fa8b6368f61f57ab181ab331f9259b7e9084692d6fa8bf30c660e7057d62f53f1efaeea3f97cf8c5f573c85b538d0f39654760666d6c653b4d5096eedd983";
export const salt = "585f093614aabe59a9e418fa39d29f87069e6e1522ac0170762e4884717032fa";

export function cleanDatabase() {
	return Account.remove({});
}

export function createAccount() {
	return Account.create({username: user.username, hash, salt});
}

export function createAxiosClient() {
	return axios.create({
		baseURL: 'http://localhost:3000',
		timeout: 30000,
		validateStatus: function (status) {
			return status < 500; // default
		}
	});
}

export function authenticate(username, password) {
	return client.post("/api/user/authenticate", {
				username, password
			})
			.then((response) => {

				client.defaults.headers = {
					cookie: response.headers['set-cookie'],
					"x-access-token": response.data.token
				};
				return response;
			});

}