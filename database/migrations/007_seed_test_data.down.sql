-- Remove test data in reverse order to respect foreign key constraints

-- Delete audit log entries
DELETE FROM audit_log WHERE user_id IN (
    'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
    'a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
    'a0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e'
);

-- Delete game session players
DELETE FROM game_session_players WHERE session_id = 'd0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e';

-- Delete game sessions
DELETE FROM game_sessions WHERE id = 'd0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e';

-- Delete game entities
DELETE FROM game_entities WHERE id IN (
    'c0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
    'c0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
    'c0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e'
);

-- Delete plot points
DELETE FROM plot_points WHERE id = 'b0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e';

-- Delete test users
DELETE FROM users WHERE id IN (
    'a0eebc99-1111-4ef8-8bb6-2d1a1f5c5e1e',
    'a0eebc99-2222-4ef8-8bb6-2d1a1f5c5e1e',
    'a0eebc99-3333-4ef8-8bb6-2d1a1f5c5e1e'
);