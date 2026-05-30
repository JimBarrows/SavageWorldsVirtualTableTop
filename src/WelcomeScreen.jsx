export default function WelcomeScreen( claims, handleLogout) {
    return (
      <div>
        <h1>Welcome!</h1>
        <p>You are logged in as: {claims.email}</p>
      </div>
    )
}
