import {library}                                        from '@fortawesome/fontawesome-svg-core'
import {faBan, faEdit, faPlus, faSave, faSync, faTrash} from '@fortawesome/free-solid-svg-icons'
import {configure}                                      from '@storybook/react'


library.add(
	faBan,
	faEdit,
	faPlus,
	faSave,
	faSync,
	faTrash
)

// automatically import all files ending in *.stories.js
const req = require.context('../stories', true, /.stories.js$/)

function loadStories () {
	req.keys().forEach(filename => req(filename))
}

configure(loadStories, module)
