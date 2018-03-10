export function logout () {
  localStorage.removeItem('token')
  return {
    type: auth_constants.LOGOUT_USER
  }
}

export function logoutAndRedirect () {
  return (dispatch, state) => {
    dispatch(logout())
    dispatch(push(null, '/login'))
  }
}