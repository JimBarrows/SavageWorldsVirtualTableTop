import Amplify from 'aws-amplify'
import 'bootstrap/dist/css/bootstrap.min.css'
import 'font-awesome/css/font-awesome.min.css'
import createHistory from 'history/createBrowserHistory'
import React from 'react'
import ReactDOM from 'react-dom'
import 'react-quill/dist/quill.snow.css'
import {Provider} from 'react-redux'
import {ConnectedRouter, routerMiddleware} from 'react-router-redux'
import {applyMiddleware, createStore} from 'redux'
import {createLogger} from 'redux-logger'
import thunkMiddleware from 'redux-thunk'
import App from './App'
import aws_exports from './aws-exports.js'
import reducers from './reducers'
import registerServiceWorker from './registerServiceWorker'

const history = createHistory()

const loggerMiddleware = createLogger()

Amplify.configure(aws_exports)

const store = createStore(
	reducers,
	applyMiddleware(
		loggerMiddleware,
		routerMiddleware(history),
		thunkMiddleware
	))


ReactDOM.render(
	<Provider store={store}>
		<ConnectedRouter history={history}>
			<App/>
		</ConnectedRouter>
	</Provider>,
	document.getElementById('root'))

registerServiceWorker()
