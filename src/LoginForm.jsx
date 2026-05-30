export default function LoginForm( handleLogin, email, setEmail, loading) {
    return (
        <>
            <p>Sign in via magic link with your email below</p>
            <form onSubmit={handleLogin}>
                <input
                    type="email"
                    placeholder="Your email"
                    value={email}
                    required={true}
                    onChange={(e) => setEmail(e.target.value)}
                />
                <button disabled={loading}>
                    {loading ? <span>Loading</span> : <span>Send magic link</span>}
                </button>
            </form>
        </>
    )
}
