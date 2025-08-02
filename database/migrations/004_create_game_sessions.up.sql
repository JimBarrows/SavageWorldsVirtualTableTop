-- Create game_sessions table for tracking game sessions
CREATE TABLE IF NOT EXISTS game_sessions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    plot_point_id UUID NOT NULL REFERENCES plot_points(id) ON DELETE CASCADE,
    gm_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    status VARCHAR(20) NOT NULL DEFAULT 'preparing' CHECK (status IN (
        'preparing', 'active', 'paused', 'completed', 'cancelled'
    )),
    session_data JSONB DEFAULT '{}'::jsonb,
    scheduled_at TIMESTAMPTZ,
    started_at TIMESTAMPTZ,
    ended_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create game_session_players junction table
CREATE TABLE IF NOT EXISTS game_session_players (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
    player_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    character_id UUID REFERENCES game_entities(id) ON DELETE SET NULL,
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    left_at TIMESTAMPTZ,
    is_active BOOLEAN DEFAULT true,
    player_notes TEXT,
    UNIQUE(session_id, player_id)
);

-- Create indexes for game_sessions
CREATE INDEX idx_game_sessions_plot_point_id ON game_sessions(plot_point_id);
CREATE INDEX idx_game_sessions_gm_id ON game_sessions(gm_id);
CREATE INDEX idx_game_sessions_status ON game_sessions(status);
CREATE INDEX idx_game_sessions_scheduled_at ON game_sessions(scheduled_at);
CREATE INDEX idx_game_sessions_created_at ON game_sessions(created_at);

-- Create indexes for game_session_players
CREATE INDEX idx_game_session_players_session_id ON game_session_players(session_id);
CREATE INDEX idx_game_session_players_player_id ON game_session_players(player_id);
CREATE INDEX idx_game_session_players_character_id ON game_session_players(character_id);
CREATE INDEX idx_game_session_players_is_active ON game_session_players(is_active);

-- Create triggers
CREATE TRIGGER update_game_sessions_updated_at BEFORE UPDATE
    ON game_sessions FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Add comments
COMMENT ON TABLE game_sessions IS 'Game sessions for plot points';
COMMENT ON TABLE game_session_players IS 'Players participating in game sessions';
COMMENT ON COLUMN game_sessions.status IS 'Current status of the game session';
COMMENT ON COLUMN game_sessions.session_data IS 'Session-specific data like initiative order, current scene, etc.';
COMMENT ON COLUMN game_session_players.character_id IS 'The character the player is using in this session';