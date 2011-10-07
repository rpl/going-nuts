package gen_server

// Message represents the opaque type of gen_server exchanged messages
type Message interface {}
// MessageContent represents the opaque type of gen_server exchanged data
type Data interface {}

// CastMessage is a asynchronous message (it doesn't need a reply)
type CastMessage struct {
	Payload Data
}

// CallMessage is a synchronous message (it waits for a reply on the ReplyChannel)
type CallMessage struct {
	Payload Data
  replyChannel ReplyMessageChannel
}

// ReplyMessage is a reply to a CallMessage
type ReplyMessage struct {
	Ok bool
	Result Data
	Error Data
}

// ControlMessage is a control message (optionally waits for a reply 
// on the ReplyChannel)
type controlMessage interface {}

type initControlMessage struct {
	Args Data
  ReplyChannel ReplyMessageChannel
}

type stopControlMessage struct {
	Args Data
  ReplyChannel ReplyMessageChannel
}

// MessageChannel is the channel of the messages sent from a client to the gen_server
type MessageChannel chan interface {} //Message
type controlChannel chan interface {} //controlMessage
type ReplyMessageChannel chan ReplyMessage

// GenServer statuses
const (
	NEW = iota;
	STARTING;
	READY;
	BUSY;
	STOPPED;
	CRASHED;
)

// GenServer is the struct type of a GenServer, contains data needed by internal functions
type GenServer struct {
  ch MessageChannel
  control_ch controlChannel
	impl IGenServerImpl
	state Data
	status int
	debug bool
}

// IGenServerImpl is the interface that GenServer implementations have to implements:
type IGenServerImpl interface {
  // Init initialize the server returning a state, optionally based on Init arguments
  Init(args Data) (bool, Data)
  // HandleCast handle cast messages
	HandleCast(msg *CastMessage)
  // HandleCall handle call messages
	HandleCall(msg *CallMessage)
}

