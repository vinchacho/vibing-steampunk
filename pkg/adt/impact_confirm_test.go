package adt

import (
	"fmt"
	"regexp"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

var impactTokenPattern = regexp.MustCompile(`^impact-confirm-[0-9a-f]{8}$`)

func TestImpactTokenIssueAndConsume(t *testing.T) {
	c := &Client{}
	url := "/sap/bc/adt/oo/classes/zcl_demo_payment"

	tok := c.IssueImpactToken(url, "update")
	if !impactTokenPattern.MatchString(tok) {
		t.Fatalf("token %q does not match %s", tok, impactTokenPattern)
	}

	if !c.consumeImpactToken(url, "update", tok) {
		t.Fatal("first consume with matching objectURL+op should succeed")
	}
	if c.consumeImpactToken(url, "update", tok) {
		t.Fatal("second consume should fail: token is single-use")
	}

	// Keying is case-insensitive on the object URL: a token issued with a
	// mixed-case URL must be consumable with the lowercase form.
	tok2 := c.IssueImpactToken("/sap/bc/adt/oo/classes/ZCL_DEMO_PAYMENT", "update")
	if !c.consumeImpactToken(url, "update", tok2) {
		t.Fatal("consume should be case-insensitive on objectURL")
	}
}

func TestImpactTokenExpiry(t *testing.T) {
	now := time.Date(2026, 8, 15, 12, 0, 0, 0, time.UTC)
	prev := impactNow
	impactNow = func() time.Time { return now }
	t.Cleanup(func() { impactNow = prev })

	c := &Client{}
	url := "/sap/bc/adt/programs/programs/zdemo_report"

	tok := c.IssueImpactToken(url, "update")
	now = now.Add(10*time.Minute + time.Second)
	if c.consumeImpactToken(url, "update", tok) {
		t.Fatal("consume should fail after the 10-minute TTL")
	}
}

func TestImpactTokenBinding(t *testing.T) {
	c := &Client{}
	urlA := "/sap/bc/adt/oo/classes/zcl_demo_a"
	urlB := "/sap/bc/adt/oo/classes/zcl_demo_b"

	tok := c.IssueImpactToken(urlA, "update")

	if c.consumeImpactToken(urlA, "delete", tok) {
		t.Fatal("token issued for (urlA, update) must be rejected for (urlA, delete)")
	}
	if c.consumeImpactToken(urlB, "update", tok) {
		t.Fatal("token issued for (urlA, update) must be rejected for (urlB, update)")
	}
	if !c.consumeImpactToken(urlA, "update", tok) {
		t.Fatal("token should still be valid for its own (objectURL, op) after rejected attempts")
	}
}

func TestImpactTokenReissueOverwrites(t *testing.T) {
	c := &Client{}
	url := "/sap/bc/adt/oo/classes/zcl_demo_reissue"

	tok1 := c.IssueImpactToken(url, "update")
	tok2 := c.IssueImpactToken(url, "update")

	if c.consumeImpactToken(url, "update", tok1) {
		t.Fatal("re-issuing for the same key must invalidate the previous token")
	}
	if !c.consumeImpactToken(url, "update", tok2) {
		t.Fatal("latest issued token should be valid")
	}
}

func TestImpactTokenConcurrency(t *testing.T) {
	c := &Client{}

	// Distinct keys: parallel issue/consume must be race-free and each
	// goroutine's own token must consume exactly once.
	var wg sync.WaitGroup
	for i := 0; i < 16; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			url := fmt.Sprintf("/sap/bc/adt/oo/classes/zcl_demo_par_%d", i)
			for j := 0; j < 50; j++ {
				tok := c.IssueImpactToken(url, "update")
				if !c.consumeImpactToken(url, "update", tok) {
					t.Errorf("goroutine %d: consume of freshly issued token failed", i)
					return
				}
			}
		}(i)
	}
	wg.Wait()

	// Shared token: exactly one of many concurrent consumers may win.
	url := "/sap/bc/adt/oo/classes/zcl_demo_shared"
	tok := c.IssueImpactToken(url, "update")
	var wins int32
	for i := 0; i < 16; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			if c.consumeImpactToken(url, "update", tok) {
				atomic.AddInt32(&wins, 1)
			}
		}()
	}
	wg.Wait()
	if wins != 1 {
		t.Fatalf("expected exactly 1 successful consume of a shared token, got %d", wins)
	}
}
