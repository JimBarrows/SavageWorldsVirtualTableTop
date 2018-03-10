export function createConstants (...constants) {
  return constants.reduce((acc, constant) => {
    acc[constant] = constant
    return acc
  }, {})
}

export function createReducer (initialState, reducerMap) {
  return (state = initialState, action) => {
    const reducer = (action && action.type) ? reducerMap[action.type] : null
    return reducer ? reducer(state, action.payload) : state
  }
}