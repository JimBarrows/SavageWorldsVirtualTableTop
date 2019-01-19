import {library}                                        from '@fortawesome/fontawesome-svg-core'
import {faBan, faEdit, faPlus, faSave, faSync, faTrash} from '@fortawesome/free-solid-svg-icons'
import Amplify                                          from 'aws-amplify'
import 'bootstrap/dist/css/bootstrap.css'
import React                                            from 'react'
import ReactDOM                                         from 'react-dom'
import 'react-quill/dist/quill.snow.css'
import {BrowserRouter}                                  from 'react-router-dom'
import App                                              from './App'
import aws_exports                                      from './aws-exports.js'
import './index.css'
import registerServiceWorker                            from './registerServiceWorker'


library.add(
	faBan,
	faEdit,
	faPlus,
	faSave,
	faSync,
	faTrash
)


Amplify.configure(aws_exports)

ReactDOM.render(
	<BrowserRouter >
		<App />
	</BrowserRouter >,

	document.getElementById('root'))

registerServiceWorker()
