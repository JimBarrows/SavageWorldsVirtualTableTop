/**
 * Created by JimBarrows on 2019-01-20.
 */
import {library}                                        from '@fortawesome/fontawesome-svg-core'
import {faBan, faEdit, faPlus, faSave, faSync, faTrash} from '@fortawesome/free-solid-svg-icons'

import {configure} from 'enzyme'
import Adapter     from 'enzyme-adapter-react-16'
import 'jest-enzyme'


library.add(
	faBan,
	faEdit,
	faPlus,
	faSave,
	faSync,
	faTrash
)
configure({adapter: new Adapter()})
