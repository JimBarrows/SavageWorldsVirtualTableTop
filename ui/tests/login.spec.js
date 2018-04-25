/**
 * Created by JimBarrows on 11/26/16.
 */
import PlotPointList from './pages/PlotPointList';
import Login from './pages/Login';
import RegistrationPage from './pages/Registration';

describe('The Login page', function () {
	const invalidUsernameOrPasswordMessage = 'Password or username are incorrect';
	const plotPointList                    = new PlotPointList(browser);
	const loginPage                        = new Login(browser);
	const registrationPage                 = new RegistrationPage(browser);

	beforeEach(function () {
		console.log('before each');
		browser.createAccount();
		loginPage.open();
	});
	it('blah', function () {
		console.log('blah');
	});
});