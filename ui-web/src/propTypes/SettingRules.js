import {arrayOf, shape, string} from 'prop-types'

export default arrayOf(shape({
															 name       : string.isRequired,
															 description: string.isRequired
														 }))
