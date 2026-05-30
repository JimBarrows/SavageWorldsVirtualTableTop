export default function AuthenticationErrorScreen( authError, setAuthError) {
    return (
      <div>
        <h1>Authentication</h1>
        <p>✗ Authentication failed</p>
        <p>{authError}</p>
        <button
          onClick={() => {
            setAuthError(null)
            window.history.replaceState({}, document.title, '/')
          }}
        >
          Return to login
        </button>
      </div>
    )
}
