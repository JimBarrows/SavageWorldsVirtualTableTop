import {library}                                        from '@fortawesome/fontawesome-svg-core'
import {faBan, faEdit, faPlus, faSave, faSync, faTrash} from '@fortawesome/free-solid-svg-icons'
import 'bootstrap/dist/css/bootstrap.css'
import React                                            from 'react'
import { createRoot }                                   from 'react-dom/client'
import 'react-quill/dist/quill.snow.css'
import App                                              from './App'
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

const container = document.getElementById('root')
const root = createRoot(container)

root.render(
	<App />
)

registerServiceWorker()
