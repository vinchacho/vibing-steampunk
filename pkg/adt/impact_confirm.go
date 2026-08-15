package adt

import (
	"crypto/rand"
	"crypto/subtle"
	"encoding/hex"
	"fmt"
	"strings"
	"sync"
	"time"
)

// impactTokenTTL is how long an issued confirmation token stays valid.
const impactTokenTTL = 10 * time.Minute

// impactTokenEntry is one live confirmation token bound to an
// (objectURL, op) key.
type impactTokenEntry struct {
	token   string
	expires time.Time
}

// impactTokenStore holds the process-lifetime confirmation tokens for
// blocked writes. The zero value is ready to use: the map is created
// lazily on first issue, under the mutex.
type impactTokenStore struct {
	mu      sync.Mutex
	entries map[string]impactTokenEntry // key: impactTokenKey(objectURL, op)
}

// impactTokenKey builds the store key. The object URL is lowercased so
// callers that differ only in URL casing address the same token; the
// operation is kept verbatim.
func impactTokenKey(objectURL, op string) string {
	return strings.ToLower(objectURL) + "|" + op
}

// IssueImpactToken mints a confirmation token for a blocked write on
// (objectURL, op) and returns it. The token is single-use, expires after
// 10 minutes (measured via impactNow so tests can pin the clock), and is
// valid only for the exact object URL and operation it was issued for.
// Re-issuing for the same key invalidates any previous token.
//
// Tokens live only in this process's memory: a restart invalidates them.
// That is the same trade-off the codebase already accepts for
// lock-to-transport context (lockTransports).
func (c *Client) IssueImpactToken(objectURL, op string) string {
	b := make([]byte, 4)
	if _, err := rand.Read(b); err != nil {
		// crypto/rand failing means the platform's CSPRNG is broken;
		// a guessable fallback token would defeat the gate.
		panic(fmt.Sprintf("adt: crypto/rand unavailable for impact token: %v", err))
	}
	token := "impact-confirm-" + hex.EncodeToString(b)
	now := impactNow()

	c.impactTokens.mu.Lock()
	defer c.impactTokens.mu.Unlock()
	if c.impactTokens.entries == nil {
		c.impactTokens.entries = make(map[string]impactTokenEntry)
	}
	// Opportunistic sweep: drop expired entries so abandoned blocks do not
	// accumulate over a long-lived server process.
	for k, e := range c.impactTokens.entries {
		if !now.Before(e.expires) {
			delete(c.impactTokens.entries, k)
		}
	}
	c.impactTokens.entries[impactTokenKey(objectURL, op)] = impactTokenEntry{
		token:   token,
		expires: now.Add(impactTokenTTL),
	}
	return token
}

// consumeImpactToken validates token against the entry stored for
// (objectURL, op). It returns true exactly once per issued token: a
// successful consume deletes the entry. Expired entries are deleted on
// sight and never match. The comparison is constant-time — the tokens are
// short, so a leaky compare would be trivially brute-forceable.
func (c *Client) consumeImpactToken(objectURL, op, token string) bool {
	key := impactTokenKey(objectURL, op)

	c.impactTokens.mu.Lock()
	defer c.impactTokens.mu.Unlock()
	e, ok := c.impactTokens.entries[key]
	if !ok {
		return false
	}
	if !impactNow().Before(e.expires) {
		delete(c.impactTokens.entries, key)
		return false
	}
	if subtle.ConstantTimeCompare([]byte(e.token), []byte(token)) != 1 {
		return false
	}
	delete(c.impactTokens.entries, key)
	return true
}
