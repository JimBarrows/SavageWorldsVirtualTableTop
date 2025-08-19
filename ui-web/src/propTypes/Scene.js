import * as PropTypes from 'prop-types'

const Character = PropTypes.shape({
	name       : PropTypes.string.isRequired,
	description: PropTypes.string
})

const Place = PropTypes.shape({
	name       : PropTypes.string.isRequired,
	description: PropTypes.string
})

const Scene = PropTypes.shape({
	id               : PropTypes.string,
	name             : PropTypes.string.isRequired,
	description      : PropTypes.string,
	dramatis_personae: PropTypes.arrayOf(Character),
	places           : PropTypes.arrayOf(Place)
})

export default Scene