import './index.css'
import { useState, useEffect } from 'react'
import { createClient } from '@supabase/supabase-js'
import { Button } from "@/components/ui/button"
import {
  Menubar,
  MenubarContent,
  MenubarGroup,
  MenubarItem,
  MenubarMenu,
  MenubarSeparator,
  MenubarShortcut,
  MenubarTrigger,
} from "@/components/ui/menubar"
import AuthenticationErrorScreen from './AuthenticationErrorScreen.jsx'
import AuthenticationSuccessScreen from './AuthenticationSuccessScreen.jsx'
import LoginForm from './LoginForm.jsx'
import VerificationScreen from './VerificationScreen.jsx'
import WelcomeScreen from './WelcomeScreen.jsx'

const supabase = createClient(
  import.meta.env.VITE_SUPABASE_URL,
  import.meta.env.VITE_SUPABASE_PUBLISHABLE_KEY
)

export default function App() {
  const [loading, setLoading] = useState(false)
  const [email, setEmail] = useState('')
  const [claims, setClaims] = useState(null)

  // Check URL params on initial render
  const params = new URLSearchParams(window.location.search)
  const hasTokenHash = params.get('token_hash')

  const [verifying, setVerifying] = useState(!!hasTokenHash)
  const [authError, setAuthError] = useState(null)
  const [authSuccess, setAuthSuccess] = useState(false)

  useEffect(() => {
    // Check if we have token_hash in URL (magic link callback)
    const params = new URLSearchParams(window.location.search)
    const token_hash = params.get('token_hash')
    const type = params.get('type')

    if (token_hash) {
      // Verify the OTP token
      supabase.auth
        .verifyOtp({
          token_hash,
          type: type || 'email',
        })
        .then(({ error }) => {
          if (error) {
            setAuthError(error.message)
          } else {
            setAuthSuccess(true)
            // Clear URL params
            window.history.replaceState({}, document.title, '/')
          }
          setVerifying(false)
        })
    }

    // Check for existing session using getClaims
    supabase.auth.getClaims().then(({ data: { claims } }) => {
      setClaims(claims)
    })

    // Listen for auth changes
    const {
      data: { subscription },
    } = supabase.auth.onAuthStateChange(() => {
      supabase.auth.getClaims().then(({ data: { claims } }) => {
        setClaims(claims)
      })
    })

    return () => subscription.unsubscribe()
  }, [])

  const handleLogin = async (event) => {
    event.preventDefault()
    setLoading(true)
    const { error } = await supabase.auth.signInWithOtp({
      email,
      options: {
        emailRedirectTo: window.location.origin,
      },
    })
    if (error) {
      alert(error.error_description || error.message)
    } else {
      alert('Check your email for the login link!')
    }
    setLoading(false)
  }

  const handleLogout = async () => {
    await supabase.auth.signOut()
    setClaims(null)
  }

    var screen = (<>Whoops</>)
    
  // Show verification state
  if (verifying) {
      screen = VerificationScreen()
  } else if (authError) {
      screen =  AuthenticationErrorScreen( authError, setAuthError)
  } else if (authSuccess && !claims) {
      screen = AuthenticationSuccessScreen()
  } else if (claims) {
      screen =  WelcomeScreen( claims, handleLogout)
  } else {
      screen = LoginForm( handleLogin, email, setEmail, loading)
  }

  // Show login form
  return (
      <>
          <header>Savage Worlds Virtual Table Top
              <nav>
              { claims ?  <Button onClick={handleLogout}>Sign Out</Button> : <></>}
              </nav>
          </header>
          <main>
              {screen}
          </main>
          <footer>
              Footer goes here
          </footer>
    </>
  )
}


